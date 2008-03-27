{
   xISO
   Copyright 1984, 1986, 1989, 1992, 2000, 2001, 2002
   Free Software Foundation, Inc.

   This file is part of xISO, made it by Yursoft.com

   xISO is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   Bison is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with Bison; see the file COPYING.  If not, write to the Free
   Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
   02111-1307, USA.
}


unit Textos;

interface

resourcestring
  rcSpaMensaje = 'Mensaje';
  rcSpaError = 'Error';
  rcSpaTamDiferentes = 'El tamaño de los ficheros es diferente.';
  rcSpaIntroducidoOK = 'El fichero se ha introducido correctamente.';
  rcSpaErrorAbrirXBE = 'Ha habido un error al abrir el XBE.';
  rcSpaParcheadoOK = 'Parcheado correctamente.';
  rcSpaIntroduceDir = 'Introduce la carpeta donde se encuentran los ficheros con los que generar la imagen xISO';
  rcSpaErrorCrearXISO = 'Error al crear la imagen.';
  rcSpaImagenCreadaOK = 'Se ha creado correctamente la imagen.';
  rcSpaParchearXBE = 'Ha ocurrido un error por lo que no se ha podido parchear el XBE.';
  rcSpaIntroduceDirEx = 'Introduce la carpeta donde extraer los ficheros';
  rcSpaFinExtraccion = 'Fin de extraccion.';
  rcSpaDVDnoXBOX = 'El CD/DVD no corresponde con uno de XBOX.';
  rcSpaFicheros = 'Ficheros';
  rcSpaCarpetas = 'Carpetas';
  rcSpaTamISO = 'Tamaño ISO';
  rcSpaImagenNoXBOX = 'El fichero no corresponde con una XISO.';
  rcSpaDesarrollado = 'Desarrollado integramente por';
  rcSpaAcercaDe = 'Acerca de xISO';
  rcSpaManejoASPI = 'Manejo ASPI:';
  rcSpaLecturaXBOX = 'Lectura de XBOX DVD FS:';
  rcSpaEscrituraXBOX = 'Creacion de XBOX DVD FS:';
  rcSpaCerrar = 'Cerrar';
  rcSpaParar = 'Parar';
  rcSpaEsperarHilo = 'Por favor espera a que se cierre todo los procesos';
  rcSpaCarpetaExtError = 'Introduce la carpeta donde quieres extraer los ficheros.';
  rcSpaInstalarASPI = 'Los controladores ASPI no se encuentran instalados o estas usando una version 4.7x.' + #13 +
    'Lee el fichero Readme.txt para saber como conseguirlos.';
  rcSpaISO9660ok = 'Se ha añadido soporte ISO 9660 a la imagen.';


               // Textos xisomaker
  rcSpaEscaneandoCarpeta = 'Escaneando carpeta... ';
  rcSpaGenerandoEntrada = 'Generando arbol de ficheros... ';
  rcSpaIntroduciendoFichero = 'Introduciendo fichero... ';
  rcSpaParadaAnormal = 'El proceso ha sido abortado.';
  rcSpaEscritoPVD = 'Escrito PVD';
  rcSpaEscritoXPVD = 'Escrito XDFS PVD';
  rcSpaInicioCreacion = 'Inicion de creacion de la imagen...';
  rcSpaFinCreacion = 'Fin de creacion de la imagen.';

               //Registro
  rcSpaRegCrearXISO = 'Crear xISO';
  rcSpaRegExtraerXISO = 'Extraer xISO';
  rcSpaRegExtraerConXISO = 'Extraer con xISO';
  rcSpaRegAbrirConXISO = 'Abrir con xISO';

               //Grabacion
  rcSpaGraNoDisco = 'No hay disco o esta lleno.';
  rcSpaGraDiscoLibre = 'Disco escribible.';
  rcSpaGraFinGrabacion = 'La grabacion se realizo correctamente.';
  rcSpaGraNoSector2048 = 'El tamaño de la imagen no corresponde con el tamaño de sectores';

               // Creacion ISO (Nuevo)
  rcSpaNuevaCarpeta = 'Nueva carpeta';
  rcSpaNombreCarpeta = 'Nombre carpeta';
  rcSpaEtiqueta = 'Etiqueta';

               // Cliente FTP
  rcSpaResolviendoURL = 'Resolviendo URL...';
  rcSpaConectando = 'Conectando...';
  rcSpaConectado = 'Conectado.';
  rcSpaDesconectando = 'Desconectando...';
  rcSpaDesconectado = 'Desconectado.';
  rcSpaNoSePudoConectar = 'No se pudo conectar.';

               // English Texts
  rcEngMensaje = 'Message';
  rcEngError = 'Error';
  rcEngTamDiferentes = 'The size of the files are different.';
  rcEngIntroducidoOK = 'The file was append correctly.';
  rcEngErrorAbrirXBE = 'XBE Open Error.';
  rcEngParcheadoOK = 'Patched Ok.';
  rcEngIntroduceDir = 'Introduce the folder where are the files with which to generate the image xISO';
  rcEngErrorCrearXISO = 'Image Creation Error.';
  rcEngImagenCreadaOK = 'Image Creation Ok.';
  rcEngParchearXBE = 'XBE Patch Error.';
  rcEngIntroduceDirEx = 'Introduce the folder where to extract the files.';
  rcEngFinExtraccion = 'End of extraction.';
  rcEngDVDnoXBOX = 'The CD/DVD is not an XBOX CD/DVD.';
  rcEngFicheros = 'Files';
  rcEngCarpetas = 'Dir';
  rcEngTamISO = 'ISO Size';
  rcEngImagenNoXBOX = 'The file does not correspond with an XISO.';
  rcEngDesarrollado = 'Development by';
  rcEngAcercaDe = 'About xISO';
  rcEngManejoASPI = 'ASPI Managment:';
  rcEngLecturaXBOX = 'Reading XBOX DVD FS:';
  rcEngEscrituraXBOX = 'Making XBOX DVD FS:';
  rcEngCerrar = 'Exit';
  rcEngParar = 'Stop';
  rcEngEsperarHilo = 'Please wait to stop all the processes';
  rcEngCarpetaExtError = 'Introduce the folder where to extract the files';
  rcEngInstalarASPI = 'ASPI drivers not found or the version is 4.7x' + #13 +
    'Please read the README.TXT on how to get them.';
  rcEngISO9660ok = 'Add ISO 9660 support to the image.';


               // xisomaker Texts
  rcEngEscaneandoCarpeta = 'Scanning folders... ';
  rcEngGenerandoEntrada = 'Generating files tree... ';
  rcEngIntroduciendoFichero = 'Adding file... ';
  rcEngParadaAnormal = 'The process has been aborted.';
  rcEngEscritoPVD = 'PVD Writed';
  rcEngEscritoXPVD = 'XDFS PVD Writed';
  rcEngInicioCreacion = 'Beginning image creation...';
  rcEngFinCreacion = 'End of image creation.';

               //Registro
  rcEngRegCrearXISO = 'Create xISO';
  rcEngRegExtraerXISO = 'Extract xISO';
  rcEngRegExtraerConXISO = 'Extract with xISO';
  rcEngRegAbrirConXISO = 'Open with xISO';

               //Grabacion
  rcEngGraNoDisco = 'No Disk or Disk Full.';
  rcEngGraDiscoLibre = 'Disk writable.';
  rcEngGraFinGrabacion = '.';
  rcEngGraNoSector2048 = 'The size of the image is not equal to the size if the sectors';

               // Creacion ISO (Nuevo)
  rcEngNuevaCarpeta = 'New Folder';
  rcEngNombreCarpeta = 'Dir Name';
  rcEngEtiqueta = 'Label';

               // Cliente FTP
  rcEngResolviendoURL = 'Resolving URL...';
  rcEngConectando = 'Connecting...';
  rcEngConectado = 'Connected.';
  rcEngDesconectando = 'Desconnecting...';
  rcEngDesconectado = 'Desconnected.';
  rcEngNoSePudoConectar = 'It could not be connect.';

implementation

end.
