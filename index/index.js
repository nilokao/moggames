import express from "express";
import path from "path";
import { readFileSync } from "fs";

const app = express();
const __dirname = path.resolve();

app.get("/", (req, res) => {
  res.sendFile(path.join(__dirname, "index/index.html"));
});

app.get("/games", (req, res) => {
  const data = readFileSync("./index/games.json", "utf8");
  res.json(JSON.parse(data));
});

app.listen(8080, () => console.log("index up na 8080"));
