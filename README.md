# bot

## How to run
Before the first run, 2 config files should be filled. They are both in the folder **Configs**.
The first file is **TG.config** that contains information needed for working of telegram bot.
The second file is **VK.config** that contains information needed for working of vkontakte bot.
Also it may be necessary to use a command *stack setup*.

After these two files are filled, the project may be started by typing *stack run* into command line while being in the project folder. The programm will ask about which of the bot should be run: Telegram or Vkontakte. You need to type desired option into command line, then the bot will be started.

## Basic structure
**/Handlers/Bot.hs** has the main logic of the bot that are same for both telegram and vkontakte. **Tg.hs** is an implementation for telegram and **Vk.hs** is an implementation for vkontakte.

**/Types/Bot.hs** has types that are same for both telegram and vkontakte.
**/Types/Vk.hs** has types needed for vkontakte.
**/Types/Tg.hs** has types needed for telegram.

**Utility.hs** has additional functions used by both bots.
