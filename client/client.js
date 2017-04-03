function createWebSocket() {
    var protocol = window.location.protocol == "https:" ? "wss://" : "ws://";
    var host = window.location.hostname || 'localhost';
    var uri = protocol + host + '/ws/';
    return new WebSocket(uri);
}

var users = [];

function refreshUsers() {
    var userList = document.querySelector("#users");
    userList.innerHTML = '';
    for(string of users.sort()) {
        var li = document.createElement("li");
        li.textContent = string;
        userList.appendChild(li);
    }
}

function onMessage(event) {
    addMessage(event.data);
    messages.scrollTop = messages.scrollTopMax;

    if(event.data.match(/^[^:]* joined/)) {
        var user = event.data.replace(/ .*/, '');
        users.push(user);
        refreshUsers();
    }

    if(event.data.match(/^[^:]* disconnected/)) {
        var user = event.data.replace(/ .*/, '');
        var idx = users.indexOf(user);
        users = users.slice(0, idx).concat(users.slice(idx + 1));
        refreshUsers();
    }

    saveHistory();
}

function initialize() {
    warnings.innerHTML = '';
    var ws = createWebSocket();

    ws.onopen = function() {
        warnings.innerHTML = '';
        ws.send('Hi! I am ' + user.value);
    };

    ws.onmessage = function(event) {
        if(event.data.match(/^Welcome! Users: /)) {
            /* Calculate the list of initial users */
            users = event.data.replace(/^Welcome! Users: /, '').split(', ');
            refreshUsers();

            ws.onmessage = onMessage;

            document.querySelector('#join-section').classList.add("hidden");
            document.querySelector('#chat-section').classList.remove("hidden");
            document.querySelector('#users-section').classList.remove("hidden");

            document.querySelector('#message-form').onsubmit = function() {
                if (text.value) {
                    ws.send(text.value);
                    text.value = '';
                    text.focus();
                }
                return false;
            }

        } else {
            warnings.innerHTML = event.data;
            ws.close();
        }
    };

    warnings.innerHTML = 'Connecting...';

    return false;
}

window.onload = function() {
    loadHistory();
    document.querySelector('#join-form').onsubmit = initialize;
    initialize();
}

function addMessage(text, style) {
    var p = document.createElement('p');
    p.innerHTML = text;
    if(style) {
        p.style = style;
    }
    messages.appendChild(p);
}

function saveHistory() {
    var messages = [...document.querySelectorAll("p")].map(p=>p.innerHTML);
    localStorage.setItem("history", JSON.stringify(messages));
}

function loadHistory() {
    try {
        var messages = JSON.parse(localStorage.getItem("history"));
        if (messages.constructor !== Array) {
            throw "Could not load history"
        }
        for(var text of messages) {
            addMessage(text, "color: grey;")
        }
    }
    catch(error) {
        localStorage.setItem("history", JSON.stringify([]));
    }
}
