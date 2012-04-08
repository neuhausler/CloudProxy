# CloudProxy

CloudProxy serves as a very simple Reverse Proxy listening on port 80 (http). It is forwarding requests to Tomcat, CouchDB and potentially other internal services.

Some use-cases:
* Make services running on Amazon EC2 accessible for applications running on Google AppEngine. Google restricts outside requests to only port 80.
* Single, "managed" entry point to services running in EC2.


## Development

CloudProxy requires Erlang R14B03.

To build CloudProxy, simply run `make`
