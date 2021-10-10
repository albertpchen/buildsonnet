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

local all = {
  "allDeclaredConstructors": true,
  "allPublicConstructors": true,
  "allDeclaredMethods": true,
  "allPublicMethods": true,
  "allDeclaredClasses": true,
  "allPublicClasses": true,
  "allDeclaredFields":true,
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
  class('root.JobDescription$', '0bitmap$1'),
  class('root.JobTable') { "allDeclaredMethods" : true },
  class('com.sun.org.apache.xerces.internal.jaxp.SAXParserFactoryImpl') + all,
  class('org.eclipse.lsp4j.jsonrpc.RemoteEndpoint') + all,
  class('org.eclipse.lsp4j.jsonrpc.Endpoint')  + all,
  class('root.BloopServer') + all,
  class('ch.epfl.scala.bsp4j.BuildServer') + all,
  class('ch.epfl.scala.bsp4j.ScalaBuildServer') + all,
  class('org.eclipse.lsp4j.jsonrpc.json.adapters.JsonElementTypeAdapter$Factory') + all {
    methods:[{name:"<init>",parameterTypes:[] }]
  },
  class('org.eclipse.lsp4j.jsonrpc.json.StreamMessageProducer') + all {
    methods:[{name:"<init>",parameterTypes:[] }]
  },
] + [
  class(className) + all {
    methods: [{ name:"<init>", parameterTypes: [] }]
  }
  for className in
  [
    'ch.epfl.scala.bsp4j.BspConnectionDetails',
    'ch.epfl.scala.bsp4j.BuildClientCapabilities',
    'ch.epfl.scala.bsp4j.BuildServerCapabilities',
    'ch.epfl.scala.bsp4j.BuildTarget',
    'ch.epfl.scala.bsp4j.BuildTargetCapabilities',
    'ch.epfl.scala.bsp4j.BuildTargetEvent',
    'ch.epfl.scala.bsp4j.BuildTargetIdentifier',
    'ch.epfl.scala.bsp4j.CleanCacheParams',
    'ch.epfl.scala.bsp4j.CleanCacheResult',
    'ch.epfl.scala.bsp4j.CompileParams',
    'ch.epfl.scala.bsp4j.CompileProvider',
    'ch.epfl.scala.bsp4j.CompileReport',
    'ch.epfl.scala.bsp4j.CompileResult',
    'ch.epfl.scala.bsp4j.CompileTask',
    'ch.epfl.scala.bsp4j.CppBuildTarget',
    'ch.epfl.scala.bsp4j.CppOptionsItem',
    'ch.epfl.scala.bsp4j.CppOptionsParams',
    'ch.epfl.scala.bsp4j.CppOptionsResult',
    'ch.epfl.scala.bsp4j.DebugProvider',
    'ch.epfl.scala.bsp4j.DebugSessionAddress',
    'ch.epfl.scala.bsp4j.DebugSessionParams',
    'ch.epfl.scala.bsp4j.DependencyModule',
    'ch.epfl.scala.bsp4j.DependencyModulesItem',
    'ch.epfl.scala.bsp4j.DependencyModulesParams',
    'ch.epfl.scala.bsp4j.DependencyModulesResult',
    'ch.epfl.scala.bsp4j.DependencySourcesItem',
    'ch.epfl.scala.bsp4j.DependencySourcesParams',
    'ch.epfl.scala.bsp4j.DependencySourcesResult',
    'ch.epfl.scala.bsp4j.Diagnostic',
    'ch.epfl.scala.bsp4j.DiagnosticRelatedInformation',
    'ch.epfl.scala.bsp4j.DidChangeBuildTarget',
    'ch.epfl.scala.bsp4j.InitializeBuildParams',
    'ch.epfl.scala.bsp4j.InitializeBuildResult',
    'ch.epfl.scala.bsp4j.InverseSourcesParams',
    'ch.epfl.scala.bsp4j.InverseSourcesResult',
    'ch.epfl.scala.bsp4j.JavacOptionsItem',
    'ch.epfl.scala.bsp4j.JavacOptionsParams',
    'ch.epfl.scala.bsp4j.JavacOptionsResult',
    'ch.epfl.scala.bsp4j.JvmBuildTarget',
    'ch.epfl.scala.bsp4j.JvmEnvironmentItem',
    'ch.epfl.scala.bsp4j.JvmRunEnvironmentParams',
    'ch.epfl.scala.bsp4j.JvmRunEnvironmentResult',
    'ch.epfl.scala.bsp4j.JvmTestEnvironmentParams',
    'ch.epfl.scala.bsp4j.JvmTestEnvironmentResult',
    'ch.epfl.scala.bsp4j.Location',
    'ch.epfl.scala.bsp4j.LogMessageParams',
    'ch.epfl.scala.bsp4j.MavenDependencyModule',
    'ch.epfl.scala.bsp4j.MavenDependencyModuleArtifact',
    'ch.epfl.scala.bsp4j.Position',
    'ch.epfl.scala.bsp4j.PublishDiagnosticsParams',
    'ch.epfl.scala.bsp4j.Range',
    'ch.epfl.scala.bsp4j.ResourcesItem',
    'ch.epfl.scala.bsp4j.ResourcesParams',
    'ch.epfl.scala.bsp4j.ResourcesResult',
    'ch.epfl.scala.bsp4j.RunParams',
    'ch.epfl.scala.bsp4j.RunProvider',
    'ch.epfl.scala.bsp4j.RunResult',
    'ch.epfl.scala.bsp4j.SbtBuildTarget',
    'ch.epfl.scala.bsp4j.ScalaBuildTarget',
    'ch.epfl.scala.bsp4j.ScalaMainClass',
    'ch.epfl.scala.bsp4j.ScalaMainClassesItem',
    'ch.epfl.scala.bsp4j.ScalaMainClassesParams',
    'ch.epfl.scala.bsp4j.ScalaMainClassesResult',
    'ch.epfl.scala.bsp4j.ScalaTestClassesItem',
    'ch.epfl.scala.bsp4j.ScalaTestClassesParams',
    'ch.epfl.scala.bsp4j.ScalaTestClassesResult',
    'ch.epfl.scala.bsp4j.ScalaTestParams',
    'ch.epfl.scala.bsp4j.ScalacOptionsItem',
    'ch.epfl.scala.bsp4j.ScalacOptionsParams',
    'ch.epfl.scala.bsp4j.ScalacOptionsResult',
    'ch.epfl.scala.bsp4j.ShowMessageParams',
    'ch.epfl.scala.bsp4j.SourceItem',
    'ch.epfl.scala.bsp4j.SourcesItem',
    'ch.epfl.scala.bsp4j.SourcesParams',
    'ch.epfl.scala.bsp4j.SourcesResult',
    'ch.epfl.scala.bsp4j.TaskFinishParams',
    'ch.epfl.scala.bsp4j.TaskId',
    'ch.epfl.scala.bsp4j.TaskProgressParams',
    'ch.epfl.scala.bsp4j.TaskStartParams',
    'ch.epfl.scala.bsp4j.TestFinish',
    'ch.epfl.scala.bsp4j.TestParams',
    'ch.epfl.scala.bsp4j.TestProvider',
    'ch.epfl.scala.bsp4j.TestReport',
    'ch.epfl.scala.bsp4j.TestResult',
    'ch.epfl.scala.bsp4j.TestStart',
    'ch.epfl.scala.bsp4j.TestTask',
    'ch.epfl.scala.bsp4j.TextDocumentIdentifier',
    'ch.epfl.scala.bsp4j.WorkspaceBuildTargetsResult',
  ]
]
