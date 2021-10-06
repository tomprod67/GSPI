export async function checkSinistre(data) {
    const response = await fetch(`/api/checkSinistre`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ sinistre: data })
    })
    console.log("la");
    return await response.json();
}
export async function createSinistre(data) {
    const response = await fetch(`/api/createSinistre`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ sinistre: data })
    })
    console.log("la");
    return await response.json();
}