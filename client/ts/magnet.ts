type magnet = [string[], number, number, number];
var magnets:magnet[] = [];
/* handle selection */
var lastX:number;
var lastY:number;
var picked:string[]=[]

function magnetsFromPoint(x, y) {
    return Array.from(document.getElementsByClassName("magnet"))
                .filter(e=>{
                    var r=e.getBoundingClientRect();
                    return x>r.left&&x<r.right&&y>r.top&&y<r.bottom
                });
}


function onPointerDown(event) {
    if(event.target.onclick) return;

    picked = magnetsFromPoint(event.pageX, event.pageY).map(e=>e.id);

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
    _pickN(N) {
        picked.splice(N); actions._move(0, 30+3);
        updateUi({});
    },
    _move(dx, dy) {
        picked.map(id=>magnets[id]).forEach(m=>{m[1]+=dx;m[2]+=dy})
    },
    flip() {
        picked.map(id=>magnets[id]).forEach(m=>m[0].push(m[0].shift()));
        updateUi({});
    },
    spread() {
        picked.slice(0).reverse().map(id=>magnets[id]).forEach((m, i)=>{m[1]+=(60+3)*(i+1)});
        updateUi({});
    },
    pick5() { actions._pickN(5) },
    pick4() { actions._pickN(4) },
    pick3() { actions._pickN(3) },
    pick2() { actions._pickN(2) },
    pick1() { actions._pickN(1) },
}

window.addEventListener("touchstart", onPointerDown);
window.addEventListener("touchmove", onPointerMove);
window.addEventListener("touchend", onPointerUp);
window.addEventListener("mousedown", onPointerDown);
window.addEventListener("mousemove", onPointerMove);
window.addEventListener("mouseup", onPointerUp);
