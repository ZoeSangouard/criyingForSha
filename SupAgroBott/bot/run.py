from Supagro.imageJFunc import ImageJ 

with ImageJ(teardown=False) as bot:
    bot.land_first_page()
    bot.Upload_file()
    bot.Image_Adjust_Threshold()
    bot.Analyse_set_Mesurments_Mesure()
    bot.Analyse_Mesure()
    bot.save()





