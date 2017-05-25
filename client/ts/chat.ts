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
function sendMessage(kind, value, target=undefined) {
    if(value.constructor.name !== "String") {
        value = JSON.stringify(value);
    }
    state.ws.send(JSON.stringify({kind: kind, value: value, target: target}));
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
        dispatchEvent(new CustomEvent("disconnect", {detail: state.username}));
        m.redraw();
    }
    state.ws.onmessage = (e) => {
        try {
            var message = JSON.parse(e.data);
            try {
                message.value = JSON.parse(message.value);
            }
            catch(SyntaxError) {
                // pass
            }
        }
        catch(SyntaxError) {
            var message:any = {kind: "socketError", value: e.data}
        }
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
var viewUserList = () => m("div#user-list", sorted(state.users).map(u=>m("div", u)));
var Chat = {
    view: () =>
        state.loggedIn
        ? [viewError(), viewActions(), viewChatLog(), viewInput(), viewUserList(), scrollToNewest()]
        : [viewError(), viewLogin()]
}
/* listeners */
addEventListener("socketError", (e:CustomEvent)=>{
    state.error = e.detail.value;
});
addEventListener("login", (e:CustomEvent)=>{
    state.loggedIn = true;
    state.users = new Set([...e.detail.value.split(';')]);
});
addEventListener("connect", (e:CustomEvent)=>{
    state.users.add(e.detail.value);
});
addEventListener("disconnect", (e:CustomEvent)=>{
    state.users.delete(e.detail.value);
});
addEventListener("post", (e:CustomEvent)=>{
    state.messages.push(`${e.detail.sender}: ${e.detail.value}`);
});
/* initialize */
loadMessagesFromStorage();
var root = document.getElementById("chat");
m.mount(root, Chat);
if(state.username) { openConnection(); }
