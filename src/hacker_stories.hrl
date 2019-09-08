-define(FETCH_PERIOD, 5*60*1000). % 5 minutes
-define(ETS_TABLE_NAME, top_stories_table).
-define(ETS_TABLE_KEY, top_stories_table).
-define(N_TOP_STORIES, 50).
-define(PAGINATION_PAGE_SIZE, 10).

%% hacker_news API
-define(REQUESTS_TIMEOUT, 4000).
