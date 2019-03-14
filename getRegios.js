trs = document.querySelectorAll("#thisTable > tbody > tr > td > a  ")
names = []
trs.forEach( tr =>  names.push(tr.innerText.normalize('NFD').replace(/[\u0300-\u036f]/g, "").toUpperCase() ) )
names