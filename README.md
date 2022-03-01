# Ánalisis espacial radiacion en colombia
<h4>Observaciones</h4>
En el presente repositorio encontraras los pasos para lograr hacer predicciones sobre la radiación solar recibida en el territorio colombiano a partir de 119 observaciones realizadas a lo largo del territorio nacional en julio de 2020. 

![alt text](https://i.imgur.com/o7iqwIG.png)

<h4>Predicciones</h4>
  Para realizar las predicciones via krigging espacial se decidió utilizar un modelo para el semivariograma del tipo Wave, con parametros psill = 0.065,
                  range = 250000,nugget = 0.36, podemos ver los resultados graficamente, donde es claro que la costa atlantica recibe en promedio mayor radiación que el centro y sur occidente del pais, lamentablemente por la falta de observaciones en el sur del pais nuestras preddiciones en ese lugar no son confiables. 

  ![alt text](https://i.imgur.com/ZBiK0LE.png)
  

