// assumes existence of `magnets` global

function onPointerDown(event) {
    if(event.target.classList.contains("magnet")) {
        target = event.target.id;
        lastX = event.pageX;
        lastY = event.pageY;
        event.preventDefault();
    }
}

function onPointerMove(event) {

}

function onPointerUp(event) {
    if(target) {
        var [dx, dy] = [event.pageX-lastX, event.pageY-lastY];
        var maxZ = Math.max(...magnets.map(([s,x,y,z])=>+z));
        var [s, x, y, z] = magnets[target];
        var data = {[target]: [s, +x+dx, +y+dy, maxZ+1]};
        target = null;
        lastX = null;
        lastY = null;
        ws.send(JSON.stringify({text: data, type: 'magnet'}));
    }
}

function onMagnet(event) {
    for(var k of Object.keys(event.detail)) {
        magnets[k] = event.detail[k];
    }
}

var target = null;
var lastX = null;
var lastY = null;
window.addEventListener("touchstart", onPointerDown);
window.addEventListener("touchmove", onPointerMove);
window.addEventListener("touchend", onPointerUp);
window.addEventListener("mousedown", onPointerDown);
window.addEventListener("mousemove", onPointerMove);
window.addEventListener("mouseup", onPointerUp);