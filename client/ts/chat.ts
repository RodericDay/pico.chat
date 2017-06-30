function sorted(set) {
    /* Combination of TypeScript, Sets, and ES5 needs a workaround */
    var sortable = [];
    set.forEach(item=>sortable.push(item));
    return sortable.sort()
}
function clear() {
    var msg = `You sure you want to delete ${state.messages.length} messages?`
    if(confirm(msg)) {
        state.messages = [];
        localStorage.messages = JSON.stringify(state.messages);
    }
}
function post(e) {
    e.preventDefault();
    if(e.target.post.value) {
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
}
/* views */
var Chat = {
    view: () => !state.loggedIn?[]:[
        m("div#chat-log", state.messages.map(s=>m.trust(marked(s).replace(/a href/g, `a target="_blank" href`)))),
        m("form", {onsubmit: post}, m("input[name=post]", {autocomplete: "off"})),
        m("div#user-list", sorted(state.users).map(u=>m("div", u))),
    ]
}
/* listeners */
addEventListener("login", (e:CustomEvent)=>{
    state.users = new Set([...e.detail.value.split(';')]);
    m.redraw();
    scrollToNewest();
});
addEventListener("connect", (e:CustomEvent)=>{
    state.users.add(e.detail.value);
    m.redraw();
});
addEventListener("disconnect", (e:CustomEvent)=>{
    state.users.delete(e.detail.value);
    m.redraw();
});
addEventListener("post", (e:CustomEvent)=>{
    state.messages.push(`${e.detail.sender}: ${e.detail.value}`);
    localStorage.messages = JSON.stringify(state.messages);
    m.redraw();
    scrollToNewest();
});
/* initialize */
state.actions.push(clear);
var chatRoot = document.getElementById("chat");
m.mount(chatRoot, Chat);
