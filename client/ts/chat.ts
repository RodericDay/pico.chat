var chatConfig = {
    wsUrl: "wss://chat.roderic.ca/ws/",
}
var state = {
    loggedIn: false,
    username: localStorage.username||"",
    users: new Set(),
    title: document.title,
    messages: [],
    messagesUnseen: 0,
    error: null,
    actions: [logout, clear],
    ws: null,
}
function login(e) {
    e.preventDefault();
    state.username = e.target.username.value;
    localStorage.username = state.username;
    openConnection();
}
function openConnection() {
    state.ws = new WebSocket(chatConfig.wsUrl);
    state.ws.onopen = (e) => {
        state.ws.send(`Hi! I am ${state.username}`);
        m.redraw();
    }
    state.ws.onclose = (e) => {
        state.loggedIn = false;
        m.redraw();
    }
    state.ws.onmessage = (e) => {
        if(e.data.match(/^Welcome!.*/)) {
            state.loggedIn = true;
            var string = e.data.split(': ')[1];
            if(string) { state.users = new Set([...string.split(', ')]); }
            window.dispatchEvent(new CustomEvent("login"));
        }
        else if(e.data.match(/^User already exists$/)) {
            state.error = `${state.username} is taken. Try a different username?`;
        }
        else if(e.data.match(/^Name cannot .+ be empty$/)) {
            state.error = e.data;
        }
        else if(e.data.match(/^\w+ joined$/)) {
            var name = e.data.match(/^\S+/)[0];
            state.users.add(name);
            window.dispatchEvent(new CustomEvent("user", {detail: name}));
        }
        else if(e.data.match(/^\w+ disconnected$/)) {
            var name = e.data.match(/^\S+/)[0];
            state.users.delete(name);
            window.dispatchEvent(new CustomEvent("user", {detail: name}));
        }
        else if(e.data.match(/^roderic: @refresh$/)) {
            refresh();
        }
        else if(e.data.match(/^roderic: @logout$/)) {
            logout();
        }
        else {
            try {
                var split = e.data.split(': ');
                var sender = split.shift();
                var data = JSON.parse(split.join(': '));
                var forMe = [undefined, state.username].includes(data.target);
                data.sender = sender;
                if(!data.type) {
                    throw "not a properly formatted message, assume human readable";
                }
                else if(forMe) {
                    window.dispatchEvent(new CustomEvent(data.type, {detail: data}));
                }
            }
            catch(error) {
                state.messagesUnseen += 1;
                document.title = state.messagesUnseen
                ? `${state.title} (${state.messagesUnseen})`
                : `${state.title}`;
                state.messages.push(e.data);
            }
        }
        localStorage.history = JSON.stringify(state.messages);
        m.redraw();
    }
}
function clear() {
    var msg = `You sure you want to delete ${state.messages.length} messages?`
    if(confirm(msg)) {
        localStorage.history = JSON.stringify([]);
        refresh();
    }
}
function logout() {
    delete localStorage.username;
    refresh();
}
function post(e) {
    e.preventDefault();
    if(e.target.post.value) {
        state.messagesUnseen = -1;
        state.ws.send(e.target.post.value);
        e.target.post.value = "";
    }
}
function refresh() {
    location.replace(location.href);
}
function scrollToNewest() {
    window.setTimeout(function(){
        var el = document.getElementById("chat-log");
        if(el) {
            el.scrollTop = el.scrollHeight;
        }
    }, 0);
    return []
}
var viewLogin = () => m("form[name=login]", {onsubmit: login}, [
        m("input[name=username]", {value: state.username, autocomplete: "off"}),
        m("button", "login"),
        state.error?m("div.error", {onclick: ()=>state.error = null}, state.error):null,
    ]);
var viewActions = () => m("div#actions", state.actions.slice(0).reverse().map(f=>
        m("button", {onclick: f}, f.name),
    ));
var viewChatLog = () => m("div#chat-log", state.messages.map(s=>m.trust(marked(s))));
var viewInput = () => m("form", {onsubmit: post}, [
        m("input[name=post]", {autocomplete: "off"}),
    ]);
var viewUserlist = () => m("div#user-list", [...state.users].sort().map(u=>m("div", u)));
var Chat = {
    view: () =>
        state.loggedIn
        ?    [viewActions(), viewChatLog(), viewInput(), viewUserlist(), scrollToNewest()]
        :    [viewLogin()]
}
try {
    state.messages = JSON.parse(localStorage.history);
    if(state.messages.constructor.name !== "Array") { throw "storage error" }
}
catch(e) {
    state.messages = [];
}
var root = document.getElementById("chat");
m.mount(root, Chat);
if(state.username) { openConnection(); }
