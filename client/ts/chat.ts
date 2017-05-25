function sorted(set) {
    /* Combination of TypeScript, Sets, and ES5 needs a workaround */
    var sortable = [];
    set.forEach(item=>sortable.push(item));
    return sortable.sort()
}
if(!Array.prototype.find) {
    /* Get around an issue with camera selection (exact) in older devices */
    Array.prototype.find = ()=>false;
}
var chatConfig = {
    wsUrl: location.href === "http://localhost:8000/"
        ? "ws://localhost:9160/"
        : "wss://chat.roderic.ca/ws/",
}
var state = {
    loggedIn: false,
    username: localStorage.username || "",
    users: new Set(),
    title: document.title,
    messages: [],
    messagesUnseen: 0,
    error: null,
    actions: [logout, clear],
    ws: null,
}
function sendMessage(kind, value) {
    state.ws.send(JSON.stringify({kind: kind, sender: state.username, value: value}));
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
        sendMessage("login", state.username);
        m.redraw();
    }
    state.ws.onclose = (e) => {
        state.loggedIn = false;
        dispatchEvent(new CustomEvent("logout"));
        m.redraw();
    }
    state.ws.onmessage = (e) => {
        try {
            var message = JSON.parse(e.data);
        }
        catch(SyntaxError) {
            var message = <any>{kind: "error", value: `JSON Error: "${e.data}"`}
        }
        console.log(message);
        dispatchEvent(new CustomEvent(message.kind, {detail: message}));
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
        sendMessage("post", e.target.post.value);
        e.target.post.value = "";
    }
}
function refresh() {
    location.replace(location.href);
}
function scrollToNewest() {
    var _ = function() {
        var el = document.getElementById("chat-log");
        if(el) { el.scrollTop = el.scrollHeight; }
    }
    window.setTimeout(_, 0);
    return []
}
function loadMessagesFromStorage() {
    try {
        state.messages = JSON.parse(localStorage.history);
        if(state.messages.constructor.name !== "Array") { throw "storage error" }
    }
    catch(e) {
        state.messages = [];
    }
}
/* views */
var viewError = () => state.error?m("div.centered.error", {onclick: ()=>state.error = null}, state.error):[];
var viewLogin = () => m("form[name=login]", {onsubmit: login}, [
        m("input[name=username]", {value: state.username, autocomplete: "off"}),
        m("button", "login"),
    ]);
var viewActions = () => m("div#actions", state.actions.slice(0).reverse().map(f=>
        m("button", {onclick: f}, f.name),
    ));
var viewChatLog = () => m("div#chat-log", state.messages.map(s=>m.trust(marked(s))));
var viewInput = () => m("form", {onsubmit: post}, [
        m("input[name=post]", {autocomplete: "off"}),
    ]);
var viewUserlist = () => m("div#user-list", sorted(state.users).map(u=>m("div", u)));
var Chat = {
    view: () =>
        state.loggedIn
        ? [viewError(), viewActions(), viewChatLog(), viewInput(), viewUserlist(), scrollToNewest()]
        : [viewError(), viewLogin()]
}
/* initialize */
addEventListener("error", (e)=>state.error = (e as any).detail.value);
addEventListener("login", (e)=>{state.loggedIn = true;});
addEventListener("user", (e)=>{
    var d = (e as any).detail;
    if(d.value === 'connected') state.users.add(d.sender);
    else if(d.value === 'disconnected') state.users.delete(d.sender);
});
addEventListener("post", (e)=>{
    var d = (e as any).detail;
    state.messages.push(`${d.sender}: ${d.value}`);
});
loadMessagesFromStorage();
var root = document.getElementById("chat");
m.mount(root, Chat);
if(state.username) { openConnection(); }
