const BASE_URL = "https://nilokao.github.io/moggames";

// ===== estatísticas do usuário =====

const STATS_KEY = "mogosoStats";

function getStats() {
    return JSON.parse(localStorage.getItem(STATS_KEY)) ?? {
        jogos: 0,
        vitorias: 0,
        derrotas: 0,
        streak: 0,
        melhorStreak: 0,
        tentativas: [0,0,0,0,0,0]
    };
}

function salvarStats(stats) {
    localStorage.setItem(STATS_KEY, JSON.stringify(stats));
}

function renderStats() {
    const s = getStats();
    document.getElementById("s-jogos").innerText = s.jogos;
    document.getElementById("s-vitorias").innerText = s.vitorias;
    document.getElementById("s-derrotas").innerText = s.derrotas;
    document.getElementById("s-winrate").innerText = (s.vitorias / s.jogos) * 100;
    document.getElementById("s-streak").innerText = s.streak;
    document.getElementById("s-melhor").innerText = s.melhorStreak;
}

function renderDistribuicao() {
    const s = getStats();
    const dist = document.getElementById("dist");
    dist.innerHTML = "";

    s.tentativas.forEach((v, i) => {
        const bar = document.createElement("div");
        bar.innerText = `${i+1}: ${"█".repeat(v)}`;
        dist.appendChild(bar);
    });
}

// ===== tutorial =====
const popup = document.getElementById("popup");
const statsPopup = document.getElementById("stats-popup");
const btnTutorial = document.querySelector(".buttons:nth-of-type(1)");
const btnStats = document.querySelector(".buttons:nth-of-type(2)");
const closeBtn = document.querySelector(".close");
const closeStats = document.getElementById("close-stats");

btnTutorial.onclick = () => {
    popup.style.display = "flex";
};

btnStats.onclick = () => {
    statsPopup.style.display = "flex";
    renderStats();
    renderDistribuicao();
};

closeBtn.onclick = () => popup.style.display = "none";
closeStats.onclick = () => statsPopup.style.display = "none";

window.onclick = e => {
    if (e.target === popup) {
        popup.style.display = "none";
    }

    if (e.target === statsPopup) {
        statsPopup.style.display = "none";
    }
};

document.addEventListener("keydown", e => {
    if (e.key === "Escape") {
        popup.style.display = "none";
        statsPopup.style.display = "none";
    }
});

// ===== user =====
if (!localStorage.getItem("mogosoUserId")) {
    localStorage.setItem("mogosoUserId", crypto.randomUUID());
}
const userId = localStorage.getItem("mogosoUserId");

// ===== estado =====
let tentativaAtual = 1;
let palavraAtual = Array(6).fill("");
let cursor = 0;
let jogoTravado = false;
let animando = false;

// ===== carregar estado =====
async function carregarPalavra() {
    const resp = await fetch(`${BASE_URL}/mogoso?userId=${userId}`);
    const estado = await resp.json();

    estado.tentativas.forEach((tentativa, idx) => {
        const linha = document.getElementById("r" + (idx + 1));
        const letras = linha.getElementsByClassName("letra");

        tentativa.split("").forEach((l, i) => {
            letras[i].innerText = l;
            letras[i].classList.add(
                estado.resultados[idx][i] === 2 ? "verde" :
                estado.resultados[idx][i] === 1 ? "amarelo" : "vermelho"
            );
        });
    });

    tentativaAtual = estado.tentativas.length + 1;

    if (estado.gameStatus !== "jogando") {
        jogoTravado = true;
        limparCursorGlobal();
        return;
    }

    renderLinhaAtual();
    habilitarCliqueCaixas();
}

window.onload = carregarPalavra;

// ===== render =====
function renderLinhaAtual() {
    const linha = document.getElementById("r" + tentativaAtual);
    if (!linha) return;

    const letras = linha.getElementsByClassName("letra");
    for (let i = 0; i < 6; i++) {
        letras[i].innerText = palavraAtual[i];
        letras[i].classList.toggle("cursor", i === cursor);
    }
}

// ===== clique nas caixas =====
function habilitarCliqueCaixas() {
    if (jogoTravado || animando) return;

    const linha = document.getElementById("r" + tentativaAtual);
    if (!linha) return;

    [...linha.getElementsByClassName("letra")].forEach((div, i) => {
        div.onclick = () => {
            if (jogoTravado || animando) return;
            cursor = i;
            renderLinhaAtual();
        };
    });
}

// ===== teclado físico =====
document.addEventListener("keydown", e => {
    if (jogoTravado || animando) return;

    if (e.key === "ArrowLeft") {
        cursor = Math.max(0, cursor - 1);
        renderLinhaAtual();
        return;
    }

    if (e.key === "ArrowRight") {
        cursor = Math.min(5, cursor + 1);
        renderLinhaAtual();
        return;
    }

    if (e.key === "Backspace") {
        if (palavraAtual[cursor]) {
            palavraAtual[cursor] = "";
        } else if (cursor > 0) {
            cursor--;
            palavraAtual[cursor] = "";
        }
        renderLinhaAtual();
        return;
    }

    if (e.key === "Enter") {
        if (palavraAtual.includes("")) {
            const linha = document.getElementById("r" + tentativaAtual);
            linha.classList.add("shake");
            toast("completa a palavra", "erro");
            setTimeout(() => linha.classList.remove("shake"), 400);
            return;
        }
        enviarPalavra();
        return;
    }

    if (/^[a-zA-Z]$/.test(e.key)) {
        palavraAtual[cursor] = e.key.toLowerCase();
        if (cursor < 5) cursor++;
        renderLinhaAtual();
    }
});

// ===== teclado virtual =====
document.querySelectorAll(".keyboard-button").forEach(btn => {
    btn.addEventListener("click", () => {
        if (jogoTravado || animando) return;

        animarTecla(btn);
        const tecla = btn.textContent.trim().toLowerCase();

        if (tecla === "enter") {
            if (!palavraAtual.includes("")) enviarPalavra();
        } 
        else if (tecla === "del") {
            if (palavraAtual[cursor]) {
                palavraAtual[cursor] = "";
            } else if (cursor > 0) {
                cursor--;
                palavraAtual[cursor] = "";
            }
            renderLinhaAtual();
        } 
        else {
            palavraAtual[cursor] = tecla;
            if (cursor < 5) cursor++;
            renderLinhaAtual();
        }
    });
});

// ===== enviar =====
async function enviarPalavra() {
    if (animando) return;
    animando = true;

    const tentativa = palavraAtual.join("");

    const resp = await fetch(`${BASE_URL}/mogoso`, {
        method: "POST",
        headers: { "Content-Type": "application/x-www-form-urlencoded" },
        body: `userId=${userId}&tentativa=${tentativa}`
    });

    const estado = await resp.json();
    const resultado = estado.resultados.at(-1);

    const linha = document.getElementById("r" + tentativaAtual);
    const letras = linha.getElementsByClassName("letra");

    for (let i = 0; i < 6; i++) {
        setTimeout(() => {
            letras[i].classList.add("flip");
            letras[i].classList.add(
                resultado[i] === 2 ? "verde" :
                resultado[i] === 1 ? "amarelo" : "vermelho"
            );
        }, i * 120);
    }

    atualizarTeclado(tentativa, resultado);
    limparCursorGlobal();

    setTimeout(() => {
        animando = false;
    }, 6 * 120 + 50);

    tentativaAtual++;
    palavraAtual = Array(6).fill("");
    cursor = 0;

    renderLinhaAtual();
    habilitarCliqueCaixas();

    if (estado.gameStatus !== "jogando") {
        jogoTravado = true;
        limparCursorGlobal();

        const stats = getStats();
        stats.jogos++;

        if (estado.gameStatus === "venceu") {
            stats.vitorias++;
            stats.streak++;
            stats.melhorStreak = Math.max(stats.melhorStreak, stats.streak);
            stats.tentativas[tentativaAtual - 2]++;
            toast("ganhou, volte amanhã", "sucesso");
        } else {
            stats.derrotas++;
            stats.streak = 0;
            toast("perdeu, tente amanhã", "erro");
        }

        salvarStats(stats);
    }
}

// ===== util =====
function limparCursorGlobal() {
    document.querySelectorAll(".letra").forEach(l =>
        l.classList.remove("cursor")
    );
}

function atualizarTeclado(tentativa, resultado) {
    document.querySelectorAll(".keyboard-button").forEach(btn => {
        tentativa.split("").forEach((l, i) => {
            if (btn.textContent.toLowerCase() === l) {
                if (resultado[i] === 2) btn.className = "keyboard-button verde";
                else if (resultado[i] === 1 && !btn.classList.contains("verde"))
                    btn.classList.add("amarelo");
                else if (!btn.classList.contains("verde") && !btn.classList.contains("amarelo"))
                    btn.classList.add("vermelho");
            }
        });
    });
}

function animarTecla(btn) {
    btn.classList.add("press");
    setTimeout(() => btn.classList.remove("press"), 100);
}

// ===== toast =====
function toast(msg, tipo = "sucesso") {
    const cont = document.getElementById("toast-container");
    const t = document.createElement("div");
    t.className = `toast ${tipo}`;
    t.innerText = msg;
    cont.appendChild(t);
    setTimeout(() => t.remove(), 3000);
}
