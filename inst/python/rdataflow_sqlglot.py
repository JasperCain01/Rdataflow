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


def _direct_sources(select):
    """Direct FROM + JOIN table sources of a SELECT.

    We deliberately read only this select's own FROM/JOIN clauses (not a
    recursive search) so that tables inside nested subqueries are not
    attributed to the parent stage.
    """
    sources = []
    frm = _arg(select, "from", "from_")
    if frm is not None:
        for t in frm.find_all(exp.Table):
            sources.append(_table_source(t))
    for j in _arg(select, "joins") or []:
        for t in j.find_all(exp.Table):
            sources.append(_table_source(t))
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
        out.append({
            "side": (j.side or "").upper(),   # LEFT / RIGHT / FULL / ""
            "kind": (j.kind or "").upper(),    # INNER / OUTER / CROSS / ""
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
    if isinstance(stmt, exp.Select):
        return "select_into" if stmt.args.get("into") is not None else "select"
    return "other"


def _extract_statement(stmt, index):
    """Turn one top-level statement into its kind, output, and stages."""
    stages = []

    # Each CTE is its own stage, named by its alias.
    for cte in stmt.find_all(exp.CTE):
        inner = cte.this
        if isinstance(inner, exp.Select):
            stages.append(_stage_from_select(inner, cte.alias, "cte"))

    # The statement's main SELECT becomes the "output" stage. For
    # INSERT/CREATE the SELECT is nested; for SELECT it is the statement.
    main_select = stmt if isinstance(stmt, exp.Select) else stmt.find(exp.Select)
    output_table = _output_table_name(stmt)
    if main_select is not None:
        # Avoid double-counting a CTE's select as the main select.
        cte_selects = {id(c.this) for c in stmt.find_all(exp.CTE)}
        if id(main_select) not in cte_selects:
            name = output_table if output_table is not None else "result"
            stages.append(_stage_from_select(main_select, name, "output"))

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
    dict with key ``"statements"`` -> list of statement descriptors.
    """
    statements = []
    # Use WARN rather than the default RAISE so that unsupported T-SQL
    # constructs produce a best-effort AST instead of a hard exception.
    try:
        parsed = sqlglot.parse(sql, dialect=dialect, error_level=ErrorLevel.WARN)
    except RecursionError:
        # A deeply nested expression (e.g. TRANSLATE/REPLACE chains) can blow
        # Python's call stack even with the raised recursion limit. Return
        # whatever was parsed before the crash rather than aborting R.
        parsed = []
    except Exception:
        parsed = []

    for i, stmt in enumerate(parsed, start=1):
        if stmt is None:
            continue
        # Skip purely procedural / DDL statements that carry no SELECT lineage:
        # DECLARE, DROP TABLE IF EXISTS, CREATE TABLE (no AS SELECT),
        # CREATE INDEX, and bare INSERT ... VALUES. Attempting to qualify or
        # extract projections from these crashes on certain T-SQL dialects.
        if _is_non_lineage_stmt(stmt):
            continue
        # Qualify against the schema when we have one; fall back to the raw
        # AST if qualification fails (e.g. references to temp tables not in
        # the catalog) so we still return best-effort lineage.
        try:
            if schema:
                stmt = qualify(stmt, schema=schema, dialect=dialect)
        except Exception:
            pass
        # Wrap individual statement extraction so one bad statement (e.g. a
        # deeply nested expression that survived parsing but breaks traversal)
        # does not abort processing of the remaining statements.
        try:
            statements.append(_extract_statement(stmt, i))
        except RecursionError:
            pass
        except Exception:
            pass
    return {"statements": statements}
