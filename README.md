# CloudProxy

## Overview

CloudProxy serves as a very simple Reverse Proxy listening on port 80 (http). It is forwarding requests to either Tomcat, CouchDB and potentially other internal services.

Some use-cases:

 * Make services running on Amazon EC2 accessible for applications running on Google AppEngine. Google restricts outside requests to port 80.
 * Single, "managed" entry point to services running in EC2.

CloudProxy is written in Erlang on top of [webmachine] (https://github.com/basho/webmachine), using [ibrowse](https://github.com/cmullaparthi/ibrowse/).
It is influenced by a webmachine [example code](https://bitbucket.org/bryan/wmexamples/) from Bryan Fink. 

## Development

CloudProxy requires Erlang R14B03.

To build CloudProxy, simply run `make`
