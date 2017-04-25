function createWebSocket() {
    var protocol = window.location.protocol == "https:" ? "wss://" : "ws://";
    var host = window.location.hostname || 'localhost';
    var uri = protocol + host + '/ws/';
    var ws = new WebSocket(uri);
    ws.onopen = onOpen;
    ws.onclose = onClose;
    ws.onmessage = onMessage;
    return ws
}

function onOpen(event) {
    ws.send('Hi! I am ' + uid);
    updateUi({});
}

function onClose(event) {
    messages.push({type: "info", sender: "server", text: "logout"});
    updateUi({});
}

function onMessage(event) {
    var components = event.data.split(': ');
    var sender = components.shift();
    var data = components.join(': ');
    try {
        var message = JSON.parse(data);
        message.sender = sender;
    }
    catch(error) {
    }

    if(sender === 'Welcome! Users') {
        users = new Set([...users, ...data.split(',')]);
        users.delete("");
        messages.push({type: "info", sender: "server", text: "login"});
    }

    else if(event.data.match(/^[^:]* joined/)) {
        var name = event.data.replace(/ .*/, '');
        users.add(name);
        messages.push({type: "info", sender: name, text: "connect"});

    }

    else if(event.data.match(/^[^:]* disconnected/)) {
        var name = event.data.replace(/ .*/, '');
        users.delete(name);
        messages.push({type: "info", sender: name, text: "disconnect"});
    }

    else if(message) {
        messages.push(message);
    }

    else {
        alert(event.data + '. Refresh page to try again.');
        messages.push({type: "error", sender: "system", text: event.data});
    }

    updateUi(messages[messages.length-1]);
}

function updateUi(lastMessage) {
    console.log(lastMessage);
    console.log("Implement a function to handle updates!");
}

var uid = Math.random().toFixed(16).slice(2, 8);
var users = new Set();
var messages = [];
var ws = createWebSocket();
