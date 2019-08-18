hacker_stories
=====

An OTP application 

HTTP Endpoints
----

### /top_stories/:story_id

Get the `stori_id` story from the 50 top stories.

### /top_stories?page=<page-number>

Get paginated top 50 stories.

### /top_stories

Get the top 50 stories.

Websocket Endpoint
---

### /top_stories/websocket

Websocket that re-sends the 50 tops stories every 5 minutes.

Build
-----

	$ make deps
	
Run
----

	$ make run
	

Build Documentation
----

	$ make doc

Tests 
-----

	$ make tests

Dockerized
----
* Build: `deps-docker`
* Test:  `tests-docker`
* Run:   `run-docker`
