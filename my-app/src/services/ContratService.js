export async function createContrat(data) {
    const response = await fetch(`/api/contrat`, {
        method: 'POST',
        headers: {'Content-Type': 'application/json'},
        body: JSON.stringify({contrat: data})
      })
    return await response.json();
}
