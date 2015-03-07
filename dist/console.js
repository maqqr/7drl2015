
function Console (width, height, gamestate, initCallback, keyFunc) {
    this.consoleWidth = width;
    this.consoleHeight = height;
    this.charWidth = 16;
    this.charHeight = 16;
    var canvas = document.createElement("canvas");
    this.canvas = canvas;
    this.ctx = canvas.getContext("2d");
    this.canvas.setAttribute("width", this.consoleWidth * this.charWidth);
    this.canvas.setAttribute("height", this.consoleHeight * this.charHeight);
    this.fontimg = document.createElement("img");
    this.fontimg.src = "terminal.png";

    this.gamestate = gamestate;

    this.offscreen = document.createElement("canvas");
    this.offscreen.width = this.charWidth;
    this.offscreen.height = this.charHeight;
    this.offscreenCtx = this.offscreen.getContext("2d");
    document.body.appendChild(canvas);

    this.hookKeyboard(keyFunc);
    
    var thiss = this;
    this.fontimg.onload = function () {
        
        thiss.scaleCanvas();
        window.onresize = function () {
            thiss.scaleCanvas();
        }
        if (initCallback !== undefined) {
            initCallback(thiss)(thiss.gamestate)();
        }
    }
}

Console.prototype.hookKeyboard = function(callback) {
    var con = this;
    document.addEventListener("keydown", function(ev) {
        ev = ev || window.event;
        con.gamestate = callback(con)(con.gamestate)(ev.keyCode)();
    }, false);
};

Console.prototype.scaleCanvas = function() {
    //var height = window.innerHeight-30;
    //var ratio = this.canvas.width / this.canvas.height;
    //var width = height * ratio;
    var width = window.innerWidth-30;
    var ratio = this.canvas.height / this.canvas.width;
    var height = width * ratio;
    this.canvas.style.width = width+"px";
    this.canvas.style.height = height+"px";
};

Console.prototype.clear = function() {
    this.ctx.fillColor = "#000";
    this.ctx.fillRect(0, 0, this.canvas.width, this.canvas.height);
};

Console.prototype.drawChar = function(ch, col, x, y) {
    if (isNaN(ch)) {
        ch = ch.charCodeAt(0);
    }

    var ix = ch % 16;
    var iy = Math.floor(ch / 16);

    var dx = x * this.charWidth;
    var dy = y * this.charHeight;

    this.offscreenCtx.fillStyle = col;
    this.offscreenCtx.fillRect(0, 0, this.offscreen.width, this.offscreen.height);
    
    this.ctx.globalCompositeOperation = "normal";
    this.ctx.drawImage(this.fontimg,
        this.charWidth * ix,
        this.charHeight * iy,
        this.charWidth, this.charHeight, dx, dy, this.charWidth, this.charHeight);
    this.ctx.globalCompositeOperation = "multiply";
    this.ctx.drawImage(this.offscreen, dx, dy);
};

Console.prototype.drawString = function(txt, col, x, y) {
    var xx = x;
    for (var i = 0; i < txt.length; i++) {
        this.drawChar(txt[i], col, xx, y);
        xx++;
    }
};
