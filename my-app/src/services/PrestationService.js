export async function showPrestation(data) {
    const response = await fetch(`/api/showPrestation`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ prestation: data })
    })
    console.log("dansservice");
    console.log(Date.now());
    return await response.json();
}