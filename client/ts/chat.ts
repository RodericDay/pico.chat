var config = {
    wsUrl: "wss://chat.roderic.ca/ws/",
}
var state = {
    loggedIn: false,
    username: localStorage.username||"",
    users: new Set(),
    title: document.title,
    messages: [],
    messagesUnseen: 0,
    errors: [],
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
    state.ws = new WebSocket(config.wsUrl);
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
        }
        else if(e.data.match(/^User already exists$/)) {
            state.errors.push(`${state.username} is taken. Try a different username?`);
        }
        else if(e.data.match(/^Name cannot .+ be empty$/)) {
            state.errors.push(e.data);
        }
        else if(e.data.match(/^\w+ joined$/)) {
            state.users.add(e.data.match(/^\S+/)[0]);
        }
        else if(e.data.match(/^\w+ disconnected$/)) {
            state.users.delete(e.data.match(/^\S+/)[0]);
        }
        else if(e.data.match(/^roderic: @refresh/)) {
            refresh();
        }
        else {
            try {
                var split = e.data.split(': ');
                var sender = split.shift();
                var data = JSON.parse(split.join(': '));
                data.sender = sender;
                if(data.type) {
                    window.dispatchEvent(new CustomEvent(data.type, {detail: data}));
                }
                else {
                    throw "not a properly formatted message, assume human readable";
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
function clear(e) {
    var msg = `You sure you want to delete ${state.messages.length} messages?`
    if(confirm(msg)) {
        localStorage.history = JSON.stringify([]);
        refresh();
    }
}
function logout(e) {
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
var viewErrors = () => m("div", state.errors.map((s,i)=>
        m("div.error", {onclick: ()=>state.errors.splice(i, 1)}, s))
    );
var viewLogin = () => m("form", {onsubmit: login}, [
        m("input[name=username]", {value: state.username, autocomplete: "off"}),
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
        ?    [viewErrors(), viewActions(), viewChatLog(), viewInput(), viewUserlist(), scrollToNewest()]
        :    [viewErrors(), viewLogin()]
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
