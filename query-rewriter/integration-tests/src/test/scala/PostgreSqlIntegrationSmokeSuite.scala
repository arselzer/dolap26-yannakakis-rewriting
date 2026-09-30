package at.ac.tuwien.dbai.yannakakis

class PostgreSqlIntegrationSmokeSuite extends munit.FunSuite {
  test("PostgreSQL integration dependencies are available") {
    assert(Class.forName("org.postgresql.Driver") != null)
    assert(Class.forName("org.testcontainers.postgresql.PostgreSQLContainer") != null)
  }
}
