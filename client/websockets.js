function createWebSocket() {
    var protocol = window.location.protocol == "https:" ? "wss://" : "ws://";
    var host = window.location.hostname || 'localhost';
    var uri = protocol + host + '/ws/';
    return new WebSocket(uri);
}

var users = [];
var unseenCount = 0;

function refreshUsers() {
    var userList = document.querySelector("#users");
    userList.innerHTML = '';
    for(string of users.sort()) {
        var li = document.createElement("li");
        li.textContent = string;
        li.onclick = handleUsernameClick;
        userList.appendChild(li);
    }
}

function refreshTitle() {
    document.title = 'Chat!';
    if(unseenCount) {
        document.title += ' (' + unseenCount +')';
    }
}

function onMessage(event) {

    var components = event.data.split(': ');
    var sender = components.shift();
    var data = components.join(': ');

    if(event.data.match(/^[^:]* joined/)) {
        var name = event.data.replace(/ .*/, '');
        users.push(name);
        refreshUsers();
    }

    else if(event.data.match(/^[^:]* disconnected/)) {
        var name = event.data.replace(/ .*/, '');
        var idx = users.indexOf(name);
        users.splice(idx, 1);
        refreshUsers();
    }

    else {
        var message = JSON.parse(data);
        if(message.target==='all' && message.text) {
            addMessage(sender + ': ' + message.text);
            unseenCount += 1;
            refreshTitle();
        }
        else if(message.target===user.value) {
            message.sender = sender;
            handleCustomMessage(message);
        }
    }

    messages.scrollTop = messages.scrollHeight;
    saveHistory();
}

function initialize() {
    warnings.innerHTML = '';

    // IT SHOULD BE VERY CLEAR WE ARE ASSIGNING A GLOBAL VARIABLE!!!
    ws = createWebSocket();

    ws.onopen = function() {
        loadHistory();
        warnings.innerHTML = '';
        ws.send('Hi! I am ' + user.value);
    };

    ws.onclose = function() {
        document.querySelector('#messages').innerHTML = '';
        users = [];
        refreshUsers();
        refreshTitle();
    }

    ws.onmessage = function(event) {
        if(event.data.match(/^Welcome! Users: /)) {
            /* Calculate the list of initial users */
            users = event.data.replace(/^Welcome! Users: /, '').split(', ');
            refreshUsers();

            ws.onmessage = onMessage;

            document.querySelector('input#text').onkeyup = function(event) {
                unseenCount = -1;

                if (event.keyCode===13 && text.value) {
                    ws.send(JSON.stringify({target: "all", text: text.value}));
                    text.value = '';
                    text.focus();
                }
            }

            document.querySelector('button#leave').onclick = function() {
                ws.close();
            }

        } else {
            warnings.innerHTML = event.data;
            ws.close();
        }
    };

    warnings.innerHTML = 'Connecting...';
}

window.onload = function() {
    document.querySelector('button#join').onclick = initialize;
    if (user.value) { initialize(); }
}

function addMessage(text, style) {
    var p = document.createElement('p');
    p.textContent = text;
    if(style) {
        p.style = style;
    }
    messages.appendChild(p);
}

function saveHistory() {
    var messages = [...document.querySelectorAll("p")].map(p=>p.textContent);
    localStorage.setItem("history", JSON.stringify(messages));
}

function loadHistory() {
    try {
        var messages = JSON.parse(localStorage.getItem("history"));
        if (messages.constructor !== Array) {
            throw "Could not load history"
        }
        for(var text of messages.slice(-100)) {
            addMessage(text, "color: grey;")
        }
    }
    catch(error) {
        localStorage.setItem("history", JSON.stringify([]));
    }
}

function handleUsernameClick() {
    console.log("Clicked on " + this.textContent + "!");
    console.log("Override `wsUserClick` to implement extra features.");
}

function handleCustomMessage(message) {
    console.log("received an unhandled private message!");
    console.log("Override `handleCustomMessage` to implement extra features.");
}
