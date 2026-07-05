"""sqlglot extraction helper for Rdataflow.

All sqlglot-specific AST traversal lives here so the R side has a single,
stable contract to depend on: ``extract_lineage(sql, schema, dialect)``
returns a plain (JSON-serialisable) dict that reticulate converts to nested
R lists. Keeping the traversal in one place also isolates sqlglot
version differences (e.g. arg keys ``from_``/``with_`` in 30.x).
"""

import sys
import sqlglot
from sqlglot import exp
from sqlglot.errors import ErrorLevel
from sqlglot.optimizer.qualify import qualify

# sqlglot builds ASTs recursively. Complex T-SQL expressions (e.g. deeply
# nested TRANSLATE/REPLACE/REPLICATE chains or large procedural scripts) can
# exceed Python's default limit of 1000 frames, crashing the R session via
# reticulate. 5000 gives enough headroom for real-world production queries
# without meaningful performance cost.
sys.setrecursionlimit(5000)


def _arg(node, *names):
    """Version-tolerant arg lookup.

    sqlglot has renamed some arg keys across versions (``from`` -> ``from_``,
    ``with`` -> ``with_``). Try each candidate name and return the first that
    is present, else ``None``.
    """
    for name in names:
        val = node.args.get(name)
        if val is not None:
            return val
    return None


def _func_name(fn):
    """Best-effort human name for a function node."""
    # Anonymous functions (dialect-specific / unknown) carry their name in
    # the ``this`` slot; built-ins expose ``sql_name()``.
    if isinstance(fn, exp.Anonymous):
        return str(fn.name).upper()
    try:
        return fn.sql_name().upper()
    except Exception:
        return type(fn).__name__.upper()


def _describe_projection(proj):
    """Summarise a single SELECT-list expression.

    Returns the output name, the raw SQL of the expression, the source
    columns it references, the function names it contains, and structural
    flags (aggregate / window / case) that the R classifier turns into a
    transformation category.
    """
    columns = [
        {"table": c.table or None, "name": c.name}
        for c in proj.find_all(exp.Column)
    ]
    funcs = [_func_name(f) for f in proj.find_all(exp.Func)]
    return {
        "output": proj.alias_or_name,
        "expr": proj.sql(dialect="tsql"),
        "columns": columns,
        "functions": funcs,
        # exp.AggFunc is the base class for SUM/COUNT/AVG/... aggregates.
        "is_aggregate": any(isinstance(f, exp.AggFunc)
                            for f in proj.find_all(exp.Func)),
        "has_window": proj.find(exp.Window) is not None,
        "has_case": proj.find(exp.Case) is not None,
    }


def _table_source(t):
    """Normalise an exp.Table into a source descriptor."""
    return {
        "catalog": t.catalog or None,
        "schema": t.db or None,
        "table": t.name,
        "alias": t.alias_or_name,
    }


def _relation_entry(node):
    """Normalise one FROM/JOIN relation into a source descriptor.

    Physical tables carry catalog/schema; derived tables (Subquery) and
    APPLY targets (Lateral) are referenced by their alias, which matches
    the name of the stage created for them by _derived_stages().
    """
    if isinstance(node, exp.Table):
        return _table_source(node)
    if isinstance(node, (exp.Subquery, exp.Lateral)):
        name = node.alias_or_name or "subquery"
        return {"catalog": None, "schema": None, "table": name, "alias": name}
    return None


def _relations(select):
    """The direct FROM/JOIN relation nodes of a SELECT, in positional order
    (FROM first, then each JOIN). Deliberately non-recursive so relations
    inside nested subqueries are not attributed to the parent stage — and so
    the positional join alignment (source i+1 <-> join i) holds."""
    rels = []
    frm = _arg(select, "from", "from_")
    if frm is not None and frm.this is not None:
        rels.append(frm.this)
    for j in _arg(select, "joins") or []:
        if j.this is not None:
            rels.append(j.this)
    return rels


def _direct_sources(select):
    """Direct FROM + JOIN sources of a SELECT (tables and derived tables)."""
    sources = []
    for rel in _relations(select):
        entry = _relation_entry(rel)
        if entry is not None:
            sources.append(entry)
    return sources


def _join_keys(on):
    """Extract column=column equalities from a JOIN ON predicate."""
    keys = []
    if on is None:
        return keys
    for eq in on.find_all(exp.EQ):
        left, right = eq.this, eq.expression
        if isinstance(left, exp.Column) and isinstance(right, exp.Column):
            keys.append({
                "left": f"{left.table}.{left.name}" if left.table else left.name,
                "right": f"{right.table}.{right.name}" if right.table else right.name,
            })
    return keys


def _joins(select):
    """Describe each JOIN of a SELECT: side/kind, ON text, and join keys."""
    out = []
    for j in _arg(select, "joins") or []:
        on = j.args.get("on")
        kind = (j.kind or "").upper()          # INNER / OUTER / CROSS / ""
        # CROSS/OUTER APPLY parses as a join onto a Lateral with empty kind;
        # label it so the diagram doesn't show a bare "JOIN".
        if not kind and isinstance(j.this, exp.Lateral):
            kind = "APPLY"
        out.append({
            "side": (j.side or "").upper(),    # LEFT / RIGHT / FULL / ""
            "kind": kind,
            "on": on.sql(dialect="tsql") if on is not None else None,
            "keys": _join_keys(on),
        })
    return out


def _group_by(select):
    """List the GROUP BY expressions of a SELECT as raw SQL strings."""
    grp = _arg(select, "group")
    if grp is None:
        return []
    return [e.sql(dialect="tsql") for e in grp.expressions]


def _where_sql(select):
    """Raw SQL of the WHERE predicate, if any."""
    where = _arg(select, "where")
    return where.this.sql(dialect="tsql") if where is not None else None


def _stage_from_select(select, name, role):
    """Build a stage descriptor from a SELECT node."""
    return {
        "name": name,
        "role": role,                       # "cte" or "output"
        "projections": [_describe_projection(p) for p in select.expressions],
        "sources": _direct_sources(select),
        "joins": _joins(select),
        "group_by": _group_by(select),
        "where": _where_sql(select),
    }


def _output_table_name(stmt):
    """Determine the materialised output table name of a statement, if any.

    Covers SELECT ... INTO, INSERT INTO, and CREATE TABLE AS SELECT. Returns
    ``None`` for a bare SELECT (an anonymous result set).
    """
    into = stmt.args.get("into")
    if into is not None:
        return into.this.sql(dialect="tsql")
    if isinstance(stmt, (exp.Insert, exp.Create)):
        target = stmt.this
        # CREATE wraps the table in a Schema node; unwrap to the table.
        if isinstance(target, exp.Schema):
            target = target.this
        if isinstance(target, exp.Table):
            return target.sql(dialect="tsql")
    return None


def _statement_kind(stmt):
    """Coarse classification of a top-level statement."""
    if isinstance(stmt, exp.Insert):
        return "insert"
    if isinstance(stmt, exp.Create):
        return "create"
    if isinstance(stmt, exp.Merge):
        return "merge"
    if isinstance(stmt, exp.Update):
        return "update"
    if isinstance(stmt, exp.Union):
        return "select"
    if isinstance(stmt, exp.Select):
        return "select_into" if stmt.args.get("into") is not None else "select"
    return "other"


def _query_selects(node):
    """Unwrap set operations into their component SELECT branches.

    A plain SELECT yields itself; UNION / EXCEPT / INTERSECT trees yield
    every leaf SELECT in order. Anything else yields nothing.
    """
    if isinstance(node, exp.Select):
        return [node]
    if isinstance(node, exp.Union):  # Except / Intersect subclass Union
        return _query_selects(node.this) + _query_selects(node.expression)
    if isinstance(node, exp.Subquery):
        return _query_selects(node.this)
    return []


def _derived_stages(select, stages, seen):
    """Recursively add stages for derived tables in `select`'s FROM/JOINs.

    Each `JOIN (SELECT ...) alias` / `FROM (SELECT ...) alias` / APPLY
    target becomes its own stage (role "subquery") named by its alias, so
    its inner tables are attributed to it — not to the consuming stage —
    and column lineage flows subquery -> consumer. Nested derived tables
    are emitted before their consumers.
    """
    for rel in _relations(select):
        if isinstance(rel, (exp.Subquery, exp.Lateral)):
            inner = rel.this
            if isinstance(inner, exp.Subquery):     # Lateral may wrap Subquery
                inner = inner.this
            name = rel.alias_or_name or "subquery"
            for k, sel in enumerate(_query_selects(inner), start=1):
                if id(sel) in seen:
                    continue
                seen.add(id(sel))
                _derived_stages(sel, stages, seen)
                branch = name if k == 1 else "%s (branch %d)" % (name, k)
                stages.append(_stage_from_select(sel, branch, "subquery"))


def _insert_columns(stmt):
    """The explicit column list of INSERT INTO t (c1, c2, ...), or None."""
    if not isinstance(stmt, exp.Insert):
        return None
    target = stmt.this
    if isinstance(target, exp.Schema) and target.expressions:
        return [e.name for e in target.expressions]
    return None


def _extract_statement(stmt, index):
    """Turn one top-level statement into its kind, output, and stages."""
    stages = []
    seen = set()

    # Each CTE is its own stage, named by its alias. A UNION inside a CTE
    # yields one stage per branch. Derived tables inside the CTE become
    # their own "subquery" stages, emitted first.
    cte_select_ids = set()
    for cte in stmt.find_all(exp.CTE):
        for k, sel in enumerate(_query_selects(cte.this), start=1):
            cte_select_ids.add(id(sel))
            if id(sel) in seen:
                continue
            seen.add(id(sel))
            _derived_stages(sel, stages, seen)
            name = cte.alias if k == 1 else "%s (branch %d)" % (cte.alias, k)
            stages.append(_stage_from_select(sel, name, "cte"))

    # The statement's main query becomes the "output" stage(s) — one per
    # UNION branch so no branch's lineage is silently dropped. For
    # INSERT/CREATE the query is the nested expression; for SELECT/UNION it
    # is the statement itself.
    if isinstance(stmt, (exp.Select, exp.Union)):
        query = stmt
    else:
        query = stmt.args.get("expression")
        if not isinstance(query, (exp.Select, exp.Union)):
            query = stmt.find(exp.Select)

    output_table = _output_table_name(stmt)
    insert_cols = _insert_columns(stmt)
    branches = [s for s in _query_selects(query)
                if id(s) not in cte_select_ids] if query is not None else []
    base_name = output_table if output_table is not None else "result"

    for k, sel in enumerate(branches, start=1):
        if id(sel) in seen:
            continue
        seen.add(id(sel))
        _derived_stages(sel, stages, seen)
        name = base_name if len(branches) == 1 \
            else "%s (branch %d)" % (base_name, k)
        stage = _stage_from_select(sel, name, "output")
        # INSERT INTO t (c1, c2) SELECT a, b: the inserted columns are the
        # INSERT list, not the SELECT aliases — rename positionally.
        if insert_cols and len(insert_cols) == len(stage["projections"]):
            for proj, col in zip(stage["projections"], insert_cols):
                proj["output"] = col
        stages.append(stage)

    return {
        "index": index,
        "kind": _statement_kind(stmt),
        "output_table": output_table,
        "stages": stages,
    }


def _is_non_lineage_stmt(stmt):
    """Return True for statements that carry no SELECT-level lineage.

    DECLARE, DROP, CREATE TABLE (without AS SELECT), CREATE INDEX, and
    bare INSERT...VALUES do not produce projections or stage connections
    that Rdataflow can trace. Skipping them avoids crashes on complex
    T-SQL procedural constructs while preserving all SELECT-bearing
    statements (SELECT...INTO, INSERT...SELECT, CREATE TABLE AS SELECT).
    """
    if isinstance(stmt, exp.Command):
        return True
    # DECLARE @var ... — procedural variable declaration
    if type(stmt).__name__ in ("Declare", "Set"):
        return True
    # DROP TABLE / DROP TABLE IF EXISTS
    if isinstance(stmt, exp.Drop):
        return True
    # CREATE INDEX (no SELECT inside)
    if isinstance(stmt, exp.Create):
        # CREATE TABLE AS SELECT is useful; bare CREATE TABLE is not
        if stmt.find(exp.Select) is None:
            return True
    # INSERT ... VALUES (no SELECT); INSERT ... SELECT is kept
    if isinstance(stmt, exp.Insert):
        if stmt.find(exp.Select) is None:
            return True
    return False


def extract_lineage(sql, schema=None, dialect="tsql"):
    """Parse a SQL script and extract per-statement lineage information.

    Parameters
    ----------
    sql : str
        The SQL script (one or more statements).
    schema : dict or None
        Nested mapping understood by sqlglot's qualifier, e.g.
        ``{"dbo": {"customers": {"customer_id": "INT"}}}``. When provided,
        ``*`` is expanded and unqualified columns are resolved.
    dialect : str
        sqlglot dialect name (default "tsql").

    Returns
    -------
    dict with keys:
      ``"statements"`` -> list of statement descriptors.
      ``"skipped"``    -> list of human-readable strings describing anything
                          that could not be parsed, qualified, or extracted.
                          Callers surface these to the user so lineage gaps
                          are never silent.
    """
    statements = []
    skipped = []
    # Use WARN rather than the default RAISE so that unsupported T-SQL
    # constructs produce a best-effort AST instead of a hard exception.
    try:
        parsed = sqlglot.parse(sql, dialect=dialect, error_level=ErrorLevel.WARN)
    except RecursionError:
        # A deeply nested expression (e.g. TRANSLATE/REPLACE chains) can blow
        # Python's call stack even with the raised recursion limit. Return
        # whatever was parsed before the crash rather than aborting R.
        skipped.append("sqlglot parse hit the recursion limit; "
                       "statement dropped")
        parsed = []
    except Exception as e:
        skipped.append("sqlglot parse failed (%s: %s)"
                       % (type(e).__name__, e))
        parsed = []

    for i, stmt in enumerate(parsed, start=1):
        if stmt is None:
            continue
        # Skip purely procedural / DDL statements that carry no SELECT lineage:
        # DECLARE, DROP TABLE IF EXISTS, CREATE TABLE (no AS SELECT),
        # CREATE INDEX, and bare INSERT ... VALUES. Attempting to qualify or
        # extract projections from these crashes on certain T-SQL dialects.
        if _is_non_lineage_stmt(stmt):
            skipped.append("no SELECT lineage in %s statement"
                           % type(stmt).__name__)
            continue
        # Qualify against the schema when we have one; fall back to the raw
        # AST if qualification fails (e.g. references to temp tables not in
        # the catalog) so we still return best-effort lineage.
        try:
            if schema:
                stmt = qualify(stmt, schema=schema, dialect=dialect)
        except Exception as e:
            skipped.append("schema qualification failed (%s); lineage may "
                           "be partial and * is not expanded"
                           % type(e).__name__)
        # Wrap individual statement extraction so one bad statement (e.g. a
        # deeply nested expression that survived parsing but breaks traversal)
        # does not abort processing of the remaining statements.
        try:
            statements.append(_extract_statement(stmt, i))
        except RecursionError:
            skipped.append("lineage extraction hit the recursion limit; "
                           "statement dropped")
        except Exception as e:
            skipped.append("lineage extraction failed (%s: %s)"
                           % (type(e).__name__, e))
    return {"statements": statements, "skipped": skipped}
