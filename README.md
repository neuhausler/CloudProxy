# CloudProxy

CloudProxy serves as a very simple Reverse Proxy forwarding requests to Tomcat and CouchDB.
Using CloudProxy makes Tomcat and CouchDB and potential other internal services accessible via port 80 (http).
Can be used to make services running on Amazon EC2 accessible for applications running on Google AppEngine, which restricts outside calls to port 80.


## Development

CloudProxy requires Erlang R14B03.

To build CloudProxy, simply run `make`
