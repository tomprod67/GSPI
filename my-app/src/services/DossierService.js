export async function checkDossier(data) {
    const response = await fetch(`/api/checkDossier`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ dossier: data })
    })
    console.log("la");
    return await response.json();
}
export async function createDossier(data) {
    const response = await fetch(`/api/createDossier`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ dossier: data })
    })
    console.log("la");
    return await response.json();
}