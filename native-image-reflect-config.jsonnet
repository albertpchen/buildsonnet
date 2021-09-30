local field(name, allowUnsafeAccess=null) = {
  name: name,
  [if std.type(allowUnsafeAccess) == 'boolean' then "allowUnsafeAccess"]: allowUnsafeAccess
};

local class(name, fields=null) =
  local fieldsType = std.type(fields);
  local hasFields = fields != null;
  local fieldsIsArray = std.type(fields) == 'array';
  {
    name: name,
    [if hasFields then 'fields']:
      if fieldsType == 'array' then
        [if std.type(f) == 'string' then field(f) else f for f in fields]
      else if fieldsType == 'string' then
        [field(fields)]
      else if fieldsType == 'object' then
        [fields]
  };

[
  class('sun.misc.Unsafe', field('theUnsafe', allowUnsafeAccess=true)),
  class('org.slf4j.helpers.NOPLoggerFactory'),
  class('java.sql.Types', [
    'BIT', 'TINYINT', 'SMALLINT', 'INTEGER', 'BIGINT', 'FLOAT', 'REAL',
    'DOUBLE', 'NUMERIC', 'DECIMAL', 'CHAR', 'VARCHAR', 'LONGVARCHAR',
    'DATE', 'TIME', 'TIMESTAMP', 'BINARY', 'VARBINARY', 'LONGVARBINARY',
    'NULL', 'OTHER', 'JAVA_OBJECT', 'DISTINCT', 'STRUCT', 'ARRAY',
    'BLOB', 'CLOB', 'REF', 'DATALINK', 'BOOLEAN', 'ROWID', 'NCHAR',
    'NVARCHAR', 'LONGNVARCHAR', 'NCLOB', 'SQLXML', 'REF_CURSOR',
    'TIME_WITH_TIMEZONE', 'TIMESTAMP_WITH_TIMEZONE',
  ]),
  class('cats.parse.Parser0', '0bitmap$1'),
  class('root.EvaluatedJObject$$anon$1', '0bitmap$1'),
  class('root.EvaluatedJObject$$anon$2', '0bitmap$2'),
  class('root.Std$$anon$1', '0bitmap$3'),
  class('root.CoursierDependency$', '0bitmap$2'),
  class('root.CoursierParams$', '0bitmap$1'),
]
