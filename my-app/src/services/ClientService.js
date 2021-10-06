export async function createClient(data) {
    const response = await fetch(`/api/client`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ client: data })
    })
    return await response.json();
}

export async function getCodeClient(data) {
    console.log("2");
    const response = await fetch(`/api/getcodeclient`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ idClient: data })
    })
    return await response.json();
}