$ wget https://repo1.maven.org/maven2/org/eclipse/jetty/aggregate/jetty-all/9.4.32.v20200930/jetty-all-9.4.32.v20200930-uber.jar
$ javac -cp jetty-all-9.4.32.v20200930-uber.jar HelloJetty.java 
$ java -cp jetty-all-9.4.32.v20200930-uber.jar:. HelloJetty

$ curl -vv localhost:8080
*   Trying ::1:8080...
* Connected to localhost (::1) port 8080 (#0)
> GET / HTTP/1.1
> Host: localhost:8080
> User-Agent: curl/7.69.1
> Accept: */*
> 
* Mark bundle as not supporting multiuse
< HTTP/1.1 200 OK
< Date: Thu, 22 Oct 2020 16:05:49 GMT
< Content-Type: text/html;charset=utf-8
< Content-Length: 21
< Server: Jetty(9.4.32.v20200930)
< 
<h1>Hello World</h1>
* Connection #0 to host localhost left intact


