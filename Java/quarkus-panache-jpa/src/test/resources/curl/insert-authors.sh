curl -s -X POST \
    -H  "Content-Type: application/json" \
    -d '
{
  "name": "Autor 1",
  "param": { "id": "1", "name": "test1"},
  "books": [
    {
      "name": "Autor 1 Libro 1"
    }, {
      "name": "Autor 1 Libro 2"
    }, {
      "name": "Autor 1 Libro 3"
    }
  ]
}
    ' \
    http://localhost:8080/api/v1/author | jq .

curl -s -X POST \
    -H  "Content-Type: application/json" \
    -d '
{
  "name": "Autor 2",
  "param": { "id": "2", "name": "test2"},
  "books": [
    {
      "name": "Autor 2 Libro 1"
    }, {
      "name": "Autor 2 Libro 2"
    }, {
      "name": "Autor 2 Libro 3"
    }

  ]
}
    ' \
    http://localhost:8080/api/v1/author | jq .

curl -s -X POST \
    -H  "Content-Type: application/json" \
    -d '
{
  "name": "Autor 3",
  "books": [
    {
      "name": "Autor 3 Libro 1"
    }, {
      "name": "Autor 3 Libro 2"
    }, {
      "name": "Autor 3 Libro 3"
    }

  ]
}
    ' \
    http://localhost:8080/api/v1/author | jq .
