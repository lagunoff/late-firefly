import * as sql from 'typescript-sql';
import * as mysql from 'mysql-promise';


function insert(table: sql.qualified_name | string, expr: sql.expression): sql.insert_expression;
function insert(table: sql.qualified_name | string, columns: sql.column_name_list, expr: sql.expression): sql.insert_expression;
function insert(table_: sql.qualified_name | string): sql.insert_expression {
  const table: sql.qualified_name = typeof(table_) !== 'string' ? table_ : { tag: 'qualified_name', catalog: null, schema: null, ident: table_ };
  const columns: sql.column_name_list|null = arguments.length === 2 ? null : arguments[1];
  const expr: sql.expression = arguments.length === 2 ? arguments[1] : arguments[2];
  return { tag: 'insert', table, columns, expr }; 
}


async function evalMysql<T>(conn: mysql.Connection, e: sql.expression): Promise<T> {
  switch (e.tag) {
    case 'insert': {
      const { table: { catalog, schema, ident }, expr, columns } = e;
      const table = [catalog, schema, ident].filter(Boolean).join('.');
      const columnsChunk = columns ? ' (' + columns.map(x => mysql.escapeId(x)).join(', ') + ')' : ' ';
      const values = await evalMysql(conn, expr);
      if (!Array.isArray(values)) throw { tag: 'Insert/expectedArray' };
      return await conn.query(`INSERT INTO ${mysql.escapeId(table)}${columnsChunk} VALUES ` + values.map(tuple => `(` + tuple.map(x => mysql.escape).join(', ') + ')').join(', '));
    }
      
    case 'select': {
      const { columns, from, where, group_by, having } = e;
      const table = [catalog, schema, ident].filter(Boolean).join('.');
      const columnsChunk = columns ? ' (' + columns.map(x => mysql.escapeId(x)).join(', ') + ')' : ' ';
      const values = await evalMysql(conn, expr);
      if (!Array.isArray(values)) throw { tag: 'Insert/expectedArray' };
      return await conn.query(`INSERT INTO ${mysql.escapeId(table)}${columnsChunk} VALUES ` + values.map(tuple => `(` + tuple.map(x => mysql.escape).join(', ') + ')').join(', '));
    }
  }
}
