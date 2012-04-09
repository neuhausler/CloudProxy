# CloudProxy

## Overview

CloudProxy serves as a very simple reverse proxy, listening on port 80 (http). It is forwarding incoming requests to either Tomcat, CouchDB, or potentially other internal services.

Some use-cases:

 * Make services running on Amazon EC2 accessible for applications running on Google AppEngine. Google restricts outside requests to port 80.
 * As a single, "managed" entry point to services running in EC2.

CloudProxy is written in Erlang based on [webmachine] (https://github.com/basho/webmachine) and [ibrowse](https://github.com/cmullaparthi/ibrowse/).
Influenced by a [example code](https://bitbucket.org/bryan/wmexamples/) for webmachine from Bryan Fink. 


## Quick Start

CloudProxy requires Erlang R14B03.

Build:

```
git clone git://github.com/neuhausler/CloudRover.git
cd CloudRover
make
```

Configure:

CloudProxy by default is listening on port 8050. Port, internal, and external URL can be configured in dispatch.conf and cloudproxy.com.

To change port from 8050 to 80:

`./priv/dispatch.conf`

```
%% Reverse Proxy CouchDB Requests
{["proxy", "couch",'*'], cloudproxy_proxy_couchdb_resource, {"http://localhost/proxy/couch/", "http://localhost:5984/"}}.

%% Reverse Proxy Tomcat Requests
{["proxy", "tomcat",'*'], cloudproxy_proxy_tomcat_resource, {"http://localhost/proxy/tomcat/", "http://localhost:8080/"}}.
```


`./priv/cloudproxy.conf`

```
{port, 80}.
```

An external server can be configure to track 404 "attacks":

`./priv/cloudproxy.conf`

```
{port, 80}.
```


Run:

```
./start.sh
```

Access to CouchDB: `http://localhost/proxy/couch/`

Access to Tomcat: `http://localhost/proxy/tomcat/`


