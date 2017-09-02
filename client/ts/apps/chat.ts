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
    }
}
function post(e) {
    e.preventDefault();
    let text = document.getElementById("chat-form")["text"];
    let [msg, target] = text.value.match(/^@(\w+) ./)||[text.value,null];
    if(target&&!state.users.has(target)) {
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
function humanize(sizeInBytes) {
    let chunk = (u,i) => [+(sizeInBytes/Math.pow(10,3*i)).toFixed(1),u];
    return ['B','KB','MB','GB'].map(chunk).filter(([n,u])=>n>1).pop().join('')
}
function refresh() {
    location.replace(location.href);
}
function uploadFile(event) {
    for(let file of event.target.files) {
        let reader = new FileReader();
        let size = humanize(file.size);
        sendMessage("post", `uploading ${file.name} (${size})...`);
        reader.onload = (e) => {
            sendMessage("fileTransfer", {data: reader.result, name: file.name, size: size});
        }
        reader.readAsBinaryString(file);
    }
}
function scrollToNewest() {
    var _ = function() {
        var el = document.getElementById("chat-log");
        if(el) { el.scrollTop = el.scrollHeight; }
    }
    window.setTimeout(_, 100);
}
function upload() {
    (document.getElementById("fileInput") as HTMLInputElement).click();
}
/* views */
var Chat = {
    view: () => !(state.loggedIn&&state.chatOn)?[]:[
        m("div#chat-log", state.messages.map(renderPost)),
        m("form#chat-form", {onsubmit: post},
          makeButton(clear),
          m("input[name=text]", {autocomplete: "off"}),
          makeButton(post),
          makeButton(upload),
        ),
        m("details#chat-userlist",
            m("summary#chat-usercount", `${state.channel||"lobby"} (${state.users.size} online)`),
            m("div", sorted(state.users).join(', ')),
        ),
        m("input#fileInput[type=file][multiple][hidden]", {onchange: uploadFile}),
    ]
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
    m.redraw();
    scrollToNewest();
});
addEventListener("fileTransfer", (e:CustomEvent)=>{
    let file = e.detail.value;
    let array = new Uint8Array(file.data.length).fill(0).map((_,i)=>file.data.charCodeAt(i));
    let blob = new Blob([array]);
    let fileUrl = URL.createObjectURL(blob);
    let fileAnchor = `<a download="${file.name}" href="${fileUrl}">${file.name} (${file.size})</a>`;
    let anchor = `${e.detail.sender}: ${fileAnchor}`;
    state.messages.push(anchor);
    m.redraw();
    scrollToNewest();
});
addEventListener("focus", (e)=>{
    document.title = state.title;
});
/* initialize */
marked.setOptions({sanitize: true});
var chatStop = () => {state.chatOn = false}
var chatStart = () => {state.chatOn = true}
state.actions.push(()=>state.chatOn?chatStop:chatStart);
var chatRoot = document.createElement("main");
chatRoot.id = "chat";
document.body.appendChild(chatRoot);
m.mount(chatRoot, Chat);
