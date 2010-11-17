# webnesia

webnesia is a REST interface to Erlang's mnesia. It provides an API and a Web frontend with many similarities to [CouchDB](http://couchdb.org).

![webnesia table UI](https://github.com/tarpipe/webnesia/raw/master/doc/images/webnesia_table_ui.png)

## Installation instructions

1. make sure you have [mochiweb](https://github.com/mochi/mochiweb) installed;

2. check the symlink to mochiweb source on `deps/`;

3. run `make`;

4. run `./start-dev.sh`;

5. open your browser on [http://localhost:8000/_utils/](http://localhost:8000/_utils/).

## API

Right now, these are the supported methods:

*   `PUT /<table_name>`: create a new table named `table_name`.

    The payload must contain the proposed table schema in the form of a JSON list of values. Example:

    `["id", "timestamp", "test_field"]`

*   `POST /<table_name>`: insert a new record into the table name `table_name` or edit an existing record.

    The payload must contain the record data in the form of a JSON key-value pair list. Example:
    
    `[{"id": 1, "timestamp": 1289145623708, "test_field": "the brown fox jumps over the lazy dog"}]`

*   `DELETE /<table_name>`: delete the table named `table_name`.

*   `DELETE /<table_name>/<record_id>`: delete a specific record from the table `table_name`.

*   `GET /<table_name>`: read information about the table `table_name`.

*   `GET /<table_name>/<record_id>`: get a specific record from the table `table_name`.

*   `GET /<table_name>/_all_records`: read all records from table `table_name`.

    There are two optional parameters:
    
    * `limit`: limit the query to a given number of records;
    * `skip`: skip a given number of records.