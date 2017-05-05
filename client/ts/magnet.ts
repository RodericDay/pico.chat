// assumes existence of `magnets` global

function onPointerDown(event) {
    picked = [...document
        .elementsFromPoint(event.pageX, event.pageY)
        .filter(e=>e.classList.contains("magnet"))
        .map(e=>e.id)
    ];
    if(picked.length > 0) {
        event.preventDefault();
        lastX = event.pageX;
        lastY = event.pageY;
    }
}

function onPointerMove(event) {
    event.preventDefault();
}

function onPointerUp(event) {
    if(picked) {
        var [dx, dy] = [event.pageX-lastX, event.pageY-lastY];
        var maxZ = Math.max(...magnets.map(([s,x,y,z])=>+z));

        var data = {};
        for(var id of picked) {
            var magnet = magnets[id];
            magnet[0].push(magnet[0].shift()) // rotate
            magnet[1] += dx;
            magnet[2] += dy;
            magnet[3] += maxZ;
            data[id] = magnet;
        }
        ws.send(JSON.stringify({text: data, type: 'magnet'}));
        picked = null;
        lastX = null;
        lastY = null;
    }
}

function onMagnet(event) {
    for(var k of Object.keys(event.detail)) {
        magnets[k] = event.detail[k];
    }
}

var picked:string[], lastX:number, lastY:number;
window.addEventListener("touchstart", onPointerDown);
window.addEventListener("touchmove", onPointerMove);
window.addEventListener("touchend", onPointerUp);
window.addEventListener("mousedown", onPointerDown);
window.addEventListener("mousemove", onPointerMove);
window.addEventListener("mouseup", onPointerUp);
