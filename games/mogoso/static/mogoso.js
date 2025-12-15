// ===== tutorial =====
const popup = document.getElementById("popup");
const btn = document.getElementById("tutorial");
const closeBtn = document.querySelector(".close");

btn.onclick = () => {
    popup.style.display = "block";
};

closeBtn.onclick = () => {
    popup.style.display = "none";
};

window.onclick = (e) => {
    if (e.target === popup) popup.style.display = "none";
};
document.addEventListener('keydown', e => {
    if (e.key === 'Escape') popup.style.display = "none";
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

// ===== carregar estado =====
async function carregarPalavra() {
    const resp = await fetch(`http://localhost:3001/mogoso?userId=${userId}`);
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

let jogoTravado = false;

// ===== clique nas caixas =====
function habilitarCliqueCaixas() {
    if (jogoTravado) return;

    const linha = document.getElementById("r" + tentativaAtual);
    if (!linha) return;

    [...linha.getElementsByClassName("letra")].forEach((div, i) => {
        div.onclick = () => {
            cursor = i;
            renderLinhaAtual();
        };
    });
}


// ===== teclado físico =====
document.addEventListener("keydown", e => {
    if (jogoTravado) return;

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

    if (e.key === " ") {
        cursor = Math.min(5, cursor + 1);
        renderLinhaAtual();
        return;
    }

    if (e.key === "Enter") {
        if (!palavraAtual.includes("")) enviarPalavra();
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
    const tentativa = palavraAtual.join("");

    const resp = await fetch("http://localhost:3001/mogoso", {
        method: "POST",
        headers: { "Content-Type": "application/x-www-form-urlencoded" },
        body: `userId=${userId}&tentativa=${tentativa}`
    });

    const estado = await resp.json();
    const resultado = estado.resultados.at(-1);

    const linha = document.getElementById("r" + tentativaAtual);
    const letras = linha.getElementsByClassName("letra");

    for (let i = 0; i < 6; i++) {
        letras[i].classList.add("flip");

        setTimeout(() => {
            letras[i].classList.add(
                resultado[i] === 2 ? "verde" :
                resultado[i] === 1 ? "amarelo" : "vermelho"
            );
            letras[i].classList.remove("flip");
        }, i * 120);
    }

    atualizarTeclado(tentativa, resultado);

    tentativaAtual++;
    palavraAtual = Array(6).fill("");
    cursor = 0;

    renderLinhaAtual();
    habilitarCliqueCaixas();

    if (estado.gameStatus !== "jogando") {
        if (estado.gameStatus !== "jogando") {
            jogoTravado = true;
            toast(
                estado.gameStatus === "venceu" ? "ganhou, volte amanhã" : "perdeu, tente amanhã",
                estado.gameStatus === "venceu" ? "sucesso" : "erro"
            );
        }
    }
}

// ===== teclado visual =====
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

// ===== toast =====
function toast(msg, tipo = "sucesso") {
    const cont = document.getElementById("toast-container");
    const t = document.createElement("div");
    t.className = `toast ${tipo}`;
    t.innerText = msg;
    cont.appendChild(t);
    setTimeout(() => t.remove(), 3000);
}
