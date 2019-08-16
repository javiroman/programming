// Express is a minimal and flexible Node.js web application framework
const express = require("express");

// Simulate a backend REST service to deliver some data in JSON format to the front-end
// https://medium.com/codingthesmartway-com-blog/create-a-rest-api-with-json-server-36da8680136d
const jsonServer = require("json-server");

// File watcher: https://github.com/paulmillr/chokidar
const chokidar = require("chokidar");

// Enable: https://en.wikipedia.org/wiki/Cross-origin_resource_sharing
// https://github.com/expressjs/cors
const cors = require("cors");

const fileName = process.argv[2] || "./data.js";
const port = process.argv[3] || 3500;

let router = undefined;

const app = express();

const createServer = () => {
    delete require.cache[require.resolve(fileName)];
    setTimeout(() => {

        router = jsonServer.router(fileName.endsWith(".js")
            ? require(fileName)() : fileName);
        }, 100)
}

createServer();

app.use(cors());
app.use(jsonServer.bodyParser);
app.use("/api", (req, resp, next) => router(req, resp, next));

chokidar.watch(fileName).on("change", () => {
    console.log("Reloading web service data ...");
    createServer();
    console.log("Reloading  web service data complete.");
});

app.listen(port, () => console.log('Web service running on port ${port}'));