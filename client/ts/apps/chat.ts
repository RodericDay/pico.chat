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
        localStorage.removeItem("messages");
    }
}
function post(e) {
    e.preventDefault();
    let text = document.getElementById("chat-form")["text"];
    let [msg, target] = text.value.match(/^@(\w+)/)||[text.value,null];
    if(target&&text.value.trim()===`@${target}`) {
        // pass, assume user still typing
    }
    else if(target&&!state.users.has(target)) {
        alert(`${target} not in channel.`)
    }
    else if(target) {//private
        sendMessage("post", text.value, target);
        sendMessage("post", text.value, state.username); // self
        text.value = `@${target} `;
    }
    else if(text.value) {//public
        sendMessage("post", text.value);
        text.value = "";
    }
}
function renderPost(string) {
    string = string.replace(/ (#\w+)/, (m, g)=>` [${g}](${g})`);
    return m.trust(marked(string).replace(/a href/g, `a target="_blank" href`))
}
function refresh() {
    location.replace(location.href);
}
function scrollToNewest() {
    var _ = function() {
        var el = document.getElementById("chat-log");
        if(el) { el.scrollTop = el.scrollHeight; }
    }
    window.setTimeout(_, 100);
}
/* views */
var Chat = {
    view: () => !state.chatOn?[]:m("div#chat", [
        m("div#chat-log", state.messages.map(renderPost)),
        m("form#chat-form", {onsubmit: post},
          m("img", {onclick: clear, src: "svg/clear.svg", title: "clear log"}),
          m("input[name=text]", {autocomplete: "off"}),
          m("img", {onclick: post, src: "svg/post.svg", title: "post message"}),
          m("img", {src: "svg/upload.svg", onclick: upload, title: "upload file"}),
        ),
        m(Upload),
        m("details#chat-userlist",
            m("summary#chat-usercount", `${state.channel||"lobby"} (${state.users.size} online)`),
            m("div", sorted(state.users).join(', ')),
        ),
    ])
}
/* listeners */
addEventListener("login", (e:CustomEvent)=>{
    let strings = e.detail.value ? e.detail.value.split(';') : [];
    state.users = new Set(strings);
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
    if(!document.hasFocus()&&document.title===state.title){document.title+=' (!)'}
    state.messages.push(`${e.detail.sender}: ${e.detail.value}`);
    localStorage.messages = JSON.stringify(state.messages);
    m.redraw();
    scrollToNewest();
});
addEventListener("focus", (e)=>{
    document.title = state.title;
});
/* initialize */
marked.setOptions({sanitize: true});
