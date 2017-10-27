function growHeight({dom:node}) {
    const {height} = node.getBoundingClientRect();
    node.ontransitionend = () => node.style = null;
    node.style.height = "0px";
    node.style.overflow = "hidden";
    node.style.transition = "height 500ms";
    setTimeout(()=>node.style.height=`${height}px`, 100);
}
function shrinkHeight({dom:node}) {
    const {height} = node.getBoundingClientRect();
    node.style.height = `${height}px`;
    node.style.overflow = "hidden";
    node.style.transition = "height 500ms";
    setTimeout(()=>node.style.height="0px", 100);
    const cleanUp = (resolve) => node.ontransitionend = resolve;
    // return promise from handler to delay deletion of node
    return new Promise(cleanUp)
}
