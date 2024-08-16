% src/hello_handler.erl
-module(hello_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    request_counter:increment(),
    {Count, RPS} = request_counter:get_stats(),
    Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"text/html">>},
        [<<"<!DOCTYPE html>
<html lang=\"en\">
<head>
    <meta charset=\"UTF-8\">
    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">
    <title>Request Counter</title>
    <style>
        body {
            background-color: black;
            color: white;
            font-family: Arial, sans-serif;
            display: flex;
            flex-direction: column;
            justify-content: center;
            align-items: center;
            height: 100vh;
            margin: 0;
            overflow: hidden;
        }
        #count {
            font-size: 15vw;
            transition: transform 0.5s ease;
        }
        #rps {
            font-size: 3vw;
            margin-top: 20px;
        }
        @keyframes pulse {
            0% { transform: scale(1); }
            50% { transform: scale(1.1); }
            100% { transform: scale(1); }
        }
    </style>
</head>
<body>
    <div id=\"count\">">>, integer_to_list(Count), <<"</div>
    <div id=\"rps\">">>, io_lib:format("~.1f", [RPS]), <<" requests/second</div>
    <script>
        var ws = new WebSocket('ws://' + window.location.host + '/ws');

        ws.onopen = function() {
            console.log('Websocket Connected')
            pingInterval = setInterval(function() {
                if (ws.readyState === WebSocket.OPEN) {
                    ws.send('ping');
                }
            }, 20000)
        }

        ws.onmessage = function(event) {
            var data = JSON.parse(event.data);
            var countElement = document.getElementById('count');
            var rpsElement = document.getElementById('rps');
            countElement.textContent = data.count;
            rpsElement.textContent = data.rps.toFixed(1) + ' requests/second';
            countElement.style.animation = 'none';
            void countElement.offsetWidth; // Trigger reflow
            countElement.style.animation = 'pulse 0.5s ease';
        };

        ws.onclose = function() {
            console.log('WebSocket disconnected');
            clearInterval(pingInterval);
        };

        ws.onerror = function(error) {
            console.error('WebSocket error:', error);
        };
    </script>
</body>
</html>">>],
        Req0),
    {ok, Req, State}.