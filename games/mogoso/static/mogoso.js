// popup do tutorial
const popup = document.getElementById("popup");
const btn = document.getElementById("tutorial");
const closeBtn = document.querySelector(".close");

btn.onclick = () => { popup.style.display = "block"; };
closeBtn.onclick = () => { popup.style.display = "none"; };
window.onclick = (e) => { if (e.target === popup) popup.style.display = "none"; };
document.addEventListener('keydown', e => { if (e.key === 'Escape') popup.style.display = "none"; });

// função que carrega uma palavra do servidor
async function carregarPalavra() {
    try {
        const resp = await fetch("http://localhost:5000/mogoso");
        const palavra = await resp.text();
    } catch (e) {
        console.error("Erro ao buscar palavra:", e);
    }
}

window.onload = carregarPalavra;

// função do teclado
function atualizarTeclado(tentativa, resultado) {
    const botoes = document.querySelectorAll(".keyboard-button");

    for (let i = 0; i < tentativa.length; i++) {
        const letra = tentativa[i].toLowerCase();
        const estado = resultado[i];

        botoes.forEach(btn => {
            const btnLetra = btn.textContent.trim().toLowerCase();
            if (btnLetra === letra) {
                if (estado === 2) {
                    btn.classList.remove("amarelo", "vermelho");
                    btn.classList.add("verde");
                } else if (estado === 1) {
                    if (!btn.classList.contains("verde")) {
                        btn.classList.remove("vermelho");
                        btn.classList.add("amarelo");
                    }
                } else if (estado === 0) {
                    if (!btn.classList.contains("verde") && !btn.classList.contains("amarelo")) {
                        btn.classList.add("vermelho");
                    }
                }
            }
        });
    }
}

let tentativaAtual = 1;

// função que vai enviar a palavra do usuário ao servidor
async function enviarPalavra() {
    const tentativaInput = document.getElementById("tentativa");
    const tentativa = tentativaInput.value.toLowerCase().trim();

    // checa se a palavra tem 6 letras
    if (tentativa.length !== 6) {
        alert("Digite uma palavra com 6 letras!");
        return;
    }

    try {
        tentativaInput.value = '';
        const resp = await fetch("http://localhost:5000/mogoso", {
            method: "POST",
            headers: { "Content-Type": "application/x-www-form-urlencoded" },
            body: "tentativa=" + encodeURIComponent(tentativa)
        });

        // json com a resposta do servidor
        const resultado = await resp.json();

        // se a resposta foi uma string é porque deu errado, ou seja, o usuário digitou uma palavra que não está no dicionário
        if (typeof resultado === "string") {
            alert(resultado);
            return;
        }

        const linha = document.getElementById("r" + tentativaAtual);
        tentativaAtual++;
        const letra = linha.getElementsByClassName("letra");

        // atualiza o teclado com base no json
        atualizarTeclado(tentativa, resultado);

        // vai colorir a tentaiva do usuário conforme as cores que o json definiu
        for (let i = 0; i < 6; i++) {
            letra[i].innerText = tentativa[i];
            letra[i].classList.remove("verde", "amarelo", "vermelho");

            if (resultado[i] === 2) letra[i].classList.add("verde");
            else if (resultado[i] === 1) letra[i].classList.add("amarelo");
            else letra[i].classList.add("vermelho");
        }

        // testes de vencedor/perdedor - venceu ou excedeu as tentativas
        let vencedor = true;
        for (let i = 0; i < 6; i++) {
            if (!letra[i].classList.contains("verde")) {
                vencedor = false;
                break;
            }
        }

        if (vencedor) {
            alert("Você venceu!");
            tentativaInput.disabled = true;
            return;
        }

        if (tentativaAtual > 7 && !vencedor) {
            alert("Você perdeu!");
            tentativaInput.disabled = true;
            return;
        }

    } catch (e) {
        console.error("Erro ao enviar palavra:", e); // tratamento de exceção
    }
}

document.getElementById("tentativa").addEventListener("keydown", function(event) {
    if (event.key === "Enter") enviarPalavra();
});

// trata do teclado virtual
function configurarTecladoVirtual() {
    const input = document.getElementById("tentativa");
    const botoes = document.querySelectorAll(".keyboard-button");

    botoes.forEach(btn => {
        btn.addEventListener("click", () => {
            const tecla = btn.textContent.trim().toLowerCase();

            if (tecla === "enter") {
                enviarPalavra();
            } 
            else if (tecla === "del") {
                input.value = input.value.slice(0, -1);
            } 
            else {
                if (input.value.length < 6) {
                    input.value += tecla;
                }
            }
        });
    });
}

window.addEventListener("DOMContentLoaded", configurarTecladoVirtual);