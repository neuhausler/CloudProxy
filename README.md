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
```bash
git clone git://github.com/neuhausler/CloudRover.git
cd CloudRover
make
```bash

Configure:

Run:
```bash
./start.sh
```bash
