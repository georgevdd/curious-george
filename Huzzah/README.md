To upload a file:

```
curl -F foo=@data/example.txt 'http://192.168.1.3/file'
```

A plain POST request won't trigger the server's file upload logic; the request must be encoded as a form (`-F`) and the data must be sent as a file (`field=@filename`).
