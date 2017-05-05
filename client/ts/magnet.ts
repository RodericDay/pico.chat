// assumes existence of `magnets` global

function onPointerDown(event) {
    if(event.target.onclick) return;
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
    if(event.target.onclick) return;
    if(picked) {
        var [dx, dy] = [event.pageX-lastX, event.pageY-lastY];
        var maxZ = Math.max(...magnets.map(([s,x,y,z])=>+z));

        actions._move(dx, dy);
        var data = {};
        for(var id of picked) {
            var magnet = magnets[id];
            magnet[3] += maxZ;
            data[id] = magnet;
        }
        ws.send(JSON.stringify({text: data, type: 'magnet'}));
    }
}

function onMagnet(event) {
    for(var k of Object.keys(event.detail)) {
        magnets[k] = event.detail[k];
    }
}

var actions = {
    _move(dx, dy) {picked.map(id=>magnets[id]).forEach(m=>{m[1]+=dx;m[2]+=dy}) },
    flip() { picked.map(id=>magnets[id]).forEach(m=>m[0].push(m[0].shift())); updateUi({}); },
    spread() { picked.slice(0).reverse().map(id=>magnets[id]).forEach((m, i)=>{m[1]+=10*i}); updateUi({}); },
    pick5() { picked.splice(5); actions._move(20, 20); updateUi({}); },
    pick4() {},
    pick3() {},
    pick2() {},
    pick1() {},
    pickN() {},
}
var picked:string[]=[], lastX:number, lastY:number;
window.addEventListener("touchstart", onPointerDown);
window.addEventListener("touchmove", onPointerMove);
window.addEventListener("touchend", onPointerUp);
window.addEventListener("mousedown", onPointerDown);
window.addEventListener("mousemove", onPointerMove);
window.addEventListener("mouseup", onPointerUp);
