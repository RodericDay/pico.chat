function humanize(sizeInBytes) {
    let chunk = (u,i) => [+(sizeInBytes/Math.pow(10,3*i)).toFixed(1),u];
    return ['B','KB','MB','GB'].map(chunk).filter(([n,u])=>n>1).pop().join(' ')
}
function uploadFile(event) {
    for(let file of event.target.files) {
        let reader = new FileReader();
        let size = humanize(file.size);
        if(file.size>1E6 && !confirm(UserStrings.largeFile)){return}
        sendMessage("post", `uploading ${file.name} (${size})...`);
        reader.onload = (e) => {
            sendMessage("fileTransfer", {data: reader.result, name: file.name, size: size});
        };
        reader.readAsBinaryString(file);
    }
}
function upload() {
    (document.getElementById("fileInput") as HTMLInputElement).click();
}
let Upload = {
    view: ()=>[
        m("input#fileInput[type=file][multiple][hidden]", {onchange: uploadFile}),
        state.uploads.map(([name, url, size], i)=>[
                m("a", {download: name, href: url}, `${name} (${size})`),
            ]
        ),
   ]
}
addEventListener("fileTransfer", (e:CustomEvent)=>{
    let file = e.detail.value;
    let array = new Uint8Array(file.data.length).fill(0).map((_,i)=>file.data.charCodeAt(i));
    let blob = new Blob([array]);
    let url = URL.createObjectURL(blob);
    state.uploads.push([file.name, url, file.size]);
    m.redraw();
});
