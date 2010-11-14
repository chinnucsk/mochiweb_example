% rr("tmp/records.hrl").
-record(sources, {data, token, user_id, input_time, content}).
-record(datasets, {url, content, duration, left_at, created_at, source_id, user_id, input_time, insert_time}).