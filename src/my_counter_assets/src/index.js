import { Actor, HttpAgent } from '@dfinity/agent';
import { idlFactory as my_counter_idl, canisterId as my_counter_id } from 'dfx-generated/my_counter';

const agent = new HttpAgent();
const my_counter = Actor.createActor(my_counter_idl, { agent, canisterId: my_counter_id });

document.getElementById("clickMeBtn").addEventListener("click", async () => {
  const name = document.getElementById("name").value.toString();
  const greeting = await my_counter.greet(name);

  document.getElementById("greeting").innerText = greeting;
});
