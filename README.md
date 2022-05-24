# bot

## How to run
Before the first run, a config file should be filled. This file is **Bot.config** which is in the folder **Configs**.
Also it may be necessary to use a command *stack setup*.

After that the project may be started from command line by typing either *stack run tg* to run Telegram bot or *stack run vk* to run Vkontakte bot while being in the project folder. 

## Basic structure
**/Handlers/Bot.hs** has the main logic of the bot that are same for both telegram and vkontakte. **Tg.hs** is an implementation for telegram and **Vk.hs** is an implementation for vkontakte.

**/Types/Bot.hs** has types that are same for both telegram and vkontakte.
**/Types/Vk.hs** has types needed for vkontakte.
**/Types/Tg.hs** has types needed for telegram.

**Utility.hs** has additional functions used by both bots.
