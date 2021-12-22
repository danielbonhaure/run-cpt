import requests
import requests.auth
import pandas


# Definición de funciones globales, en lenguaje Python.
# Función para acceder a un servicio web definido por una URL utilizando el método GET.
# Devuelve la respuesta como un pandas.DataFrame.
def consumir_servicio_GET(url, usuario, clave):
    respuesta = requests.get(url=url, auth=requests.auth.HTTPBasicAuth(usuario, clave))
    return respuesta


# Función para acceder a un servicio web definido por una URL utilizando
# un usuario y clave.
# Asumiendo que la respuesta es un string JSON, se hace la conversión
# de este string a un Data Frame.
def consumir_servicio_JSON(url, usuario, clave):
    respuesta = consumir_servicio_GET(url, usuario, clave)
    return pandas.json_normalize(respuesta.json())


base_url = 'http://192.168.100.224:8080'
usuario_default = '********'
clave_default = '********'


estaciones = consumir_servicio_JSON(url=base_url + "/estaciones",
                                    usuario=usuario_default, clave=clave_default)

estaciones = consumir_servicio_JSON(url=base_url + "/registros_mensuales/estaciones_completas/1991/2020",
                                    usuario=usuario_default, clave=clave_default)
