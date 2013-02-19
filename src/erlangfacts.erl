%%% erlangfacts get a random erlang fact!
-module(erlangfacts).
-export([getrandom/0]).

getrandom() ->
    random:seed(erlang:now()),
    Facts = [<<"Amazon uses Erlang to implement SimpleDB, providing database services as a part of the Amazon Elastic Compute Cloud (EC2).">>,
            <<"Yahoo! uses Erlang in its social bookmarking service, Delicious, which has more than 5 million users and 150 million bookmarked URLs.">>,
            <<"Facebook uses Erlang to power the backend of its chat service, handling more than 100 million active users.">>,
            <<"T-Mobile uses Erlang in its SMS and authentication systems.">>,
            <<"Motorola is using Erlang in call processing products in the public-safety industry.">>,
            <<"Ericsson uses Erlang in its support nodes, used in GPRS and 3G mobile networks worldwide.">>,
            <<"One great Erlang application is the 3D subdivision modeler Wings 3D, used to model and texture polygon meshes.">>,
            <<"Another great Erlang application is the Ejabberd system, which provides an Extensible Messaging and Presence Protocol (XMPP) based instant messaging (IM) application server.">>,
            <<"Did you know CouchDB is written in Erlang? CouchDB is a “schema-less” document-oriented database, providing scalability across multicore and multiserver clusters.">>,
            <<"The MochiWeb library provides support for building lightweight HTTP servers. It is used to power services such as MochiBot and MochiAds, which serve dynamically generated content to millions of viewers daily.  AND it's written in Erlang!">>,
            <<"Also written in Erlang, RabbitMQ, an AMQP messaging protocol implementation. AMQP is an emerging standard for high-performance enterprise messaging.">>,
            <<"Erlang Pro: Horizontal scalability (ability to distribute jobs across machine boundaries easily through seamless intra & inter machine communications). The built-in database (Mnesia) is also distributed by nature.">>,
            <<"Erlang Pro: Vertical scalability (ability to distribute jobs across processing resources on the same machine): SMP is handled natively.">>,
            <<"Erlang Pro: Code Hot-Swapping: the ability to update/upgrade code live during operations">>,
            <<"Asynchronous: the real world is async so Erlang was built to account for this basic nature. One feature that contributes to this requirement: Erlang's free processes (>32000 can run concurrently).">>,
            <<"Erlang Pro: Supervision: many different strategies for process supervision with restart strategies, thresholds etc. Helps recover from corner-cases/overloading more easily whilst still maintaining traces of the problems for later trouble-shooting, post-mortem analysis etc.">>,
            <<"Erlang Pro: Resource Management: scheduling strategies, resource monitoring etc. Note that the default process scheduler operates with O(1) scaling.">>,
            <<"Erlang Pro: Live debugging: the ability to log into live nodes at will helps trouble-shooting activities. Debugging can be undertaken live with full access to any process' running state. Also the built-in error reporting tools are very useful (but sometimes somewhat awkward to use).">>,
            <<"You should check out http://learnyousomeerlang.com.  It's a great free Erlang tutorial!">>,
            <<"Yahoo!'s Harvester was originally written in Perl, but it was re-written in Erlang. Erlang's high-level concurrency constructs -- along with OTP design principles -- make it an ideal platform for building reliable, fault tolerant, and scalable applications like Harvester. The resulting service is more scalable, available, reliable, and able to comply with tighter SLAs on a lighter code base and less expensive development efforts.">>,
            <<"Erlang.org isn't bad for documentation, but I prefer erldocs.com">>,
            <<"Erlang is a programming language used to build massively scalable soft real-time systems with requirements on high availability.">>,
            <<"Riak is a distributed, fault tolerant, open source database that illustrates how to build large scale systems using Erlang/OTP. Thanks in large part to Erlang's support for massively scalable distributed systems, Riak offers features that are uncommon in databases, such as high-availability and linear scalability of both capacity and throughput.">>,
        <<"Hosted Chef's server API is being ported from Ruby/CouchDB to Erlang/MySQL.">>],
        lists:nth(random:uniform(length(Facts)), Facts).
