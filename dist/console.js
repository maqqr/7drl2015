function rndInt (maxnum) {
    return Math.floor(Math.random() * maxnum);
}

function bresenhamjs(x0, y0, x1, y1) {
    var result = [];
    var dx = Math.abs(x1 - x0), sx = x0 < x1 ? 1 : -1;
    var dy = Math.abs(y1 - y0), sy = y0 < y1 ? 1 : -1;
    var err = (dx>dy ? dx : -dy)/2;

    while (true) {
        //result.push([x0, y0]);
        result.push({x:x0, y:y0});
        if (x0 === x1 && y0 === y1) break;
        var e2 = err;
        if (e2 > -dx) { err -= dy; x0 += sx; }
        if (e2 < dy) { err += dx; y0 += sy; }
    }
    return result;
}

function Console (width, height, gamestate, callbacks) {
    this.consoleWidth = width;
    this.consoleHeight = height;
    this.charWidth = 16;
    this.charHeight = 16;
    this.stage = new PIXI.Stage(0x000000);
    this.renderer = PIXI.autoDetectRenderer(this.charWidth * this.consoleWidth,
                                            this.charHeight * this.consoleHeight);
    document.body.appendChild(this.renderer.view);
    this.gamestate = gamestate;
    this.font = PIXI.BaseTexture.fromImage("https://dl.dropboxusercontent.com/u/51067730/terminal.png", true, PIXI.scaleModes.NEAREST);

    this.initCharMap();
    this.initCells();
    this.hookKeyboard(callbacks["onKeyPress"]);
    this.onUpdate = callbacks["onUpdate"];
    this.lastTime = Date.now();
    requestAnimFrame($.proxy(function() { this.loop() }, this));

    this.scaleCanvas();
    var thiss = this;
    window.onresize = function () {
        thiss.scaleCanvas();
    };
}

Console.prototype.scaleCanvas = function() {
    //console.log(this.renderer.view);
    var width = window.innerWidth-30;
    var ratio = this.renderer.view.height / this.renderer.view.width;
    var height = width * ratio;
    this.renderer.view.style.width = width+"px";
    this.renderer.view.style.height = height+"px";
};

Console.prototype.initCharMap = function() {
    this.chars = [];
    for (var x = 0; x < 16; x++) {
        for (var y = 0; y < 16; y++) {
            var rect = new PIXI.Rectangle(x * this.charWidth, y * this.charHeight, this.charWidth, this.charHeight);
            this.chars[x + y * 16] = new PIXI.Texture(this.font, rect);
        }
    }
};

Console.prototype.hookKeyboard = function(callback) {
    var con = this;
    document.addEventListener("keydown", function(ev) {
        ev = ev || window.event;
        if (callback !== undefined) {
            console.log(ev.keyCode);
            con.gamestate = callback(con)(con.gamestate)(ev.keyCode)();
        }
    }, false);
};

Console.prototype.initCells = function() {
    this.cells = [];
    for (var x = 0; x < this.consoleWidth; x++) {
        this.cells[x] = [];
        for ( var y = 0; y < this.consoleHeight; y++) {
            var cell = new PIXI.Sprite(this.chars[32]);
            cell.position.x = x * this.charWidth;
            cell.position.y = y * this.charHeight;
            cell.width = this.charWidth;
            cell.height = this.charHeight;
            cell.tint = 0xFFFFFF;
            this.cells[x][y] = cell;
            this.stage.addChild(cell);
        }
    }
};

Console.prototype.clear = function() {
    for (var x = 0; x < this.consoleWidth; x++) {
        for ( var y = 0; y < this.consoleHeight; y++) {
            this.cells[x][y].texture = this.chars[32];
        }
    }
};

Console.prototype.drawChar = function(ch, col, x, y) {
    if (x < 0 || y < 0 || x >= this.consoleWidth || y >= this.consoleHeight) {
        return;
    }
    
    ch = ch.charCodeAt(0);
    
    this.cells[x][y].texture = this.chars[ch];
    this.cells[x][y].tint = parseInt(col, 16);
};

Console.prototype.drawString = function(txt, col, x, y) {
    var xx = x;
    var yy = y;
    var startx = x;
    for (var i = 0; i < txt.length; i++) {
        if (txt[i] == "\n") {
            xx = startx;
            yy++;
        }
        else {
            this.drawChar(txt[i], col, xx, yy);
            xx++;
        }
    }
};

Console.prototype.loop = function() {
    var deltaTime = (new Date().getTime() - this.lastTime) / 1000;
    this.lastTime = Date.now();
    this.gamestate = this.onUpdate(this)(deltaTime)(this.gamestate)();
    this.renderer.render(this.stage);
    requestAnimFrame($.proxy(function() { this.loop() }, this));
};
