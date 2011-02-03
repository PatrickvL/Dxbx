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


unit TextConsts;

interface

resourcestring
  rcSpaMensaje = 'Mensaje';
  rcSpaError = 'Error';
  rcSpaTamDiferentes = 'El tamaño de los ficheros es diferente.';
  rcSpaIntroducidoOK = 'El fichero se ha introducido correctamente.';
  rcSpaErrorAbrirXBE = 'Ha habido un error al abrir el XBE.';
  rcSpaParcheadoOK = 'Parcheado correctamente.';
  rcSpaIntroduceDir = 'Introduce la carpeta donde se encuentran los ficheros con los que generar la imagen xISO';  rcSpaErrorCrearXISO = 'Error al crear la imagen.';
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
  SMessage = 'Message';
  SError = 'Error';
  STamDiferentes = 'The size of the files are different.';
  SIntroducidoOK = 'The file was appended correctly.';
  SErrorAbrirXBE = 'XBE Open Error.';
  SParcheadoOK = 'Patched Ok.';
  SSelectFolder = 'Select the folder with files with which to generate the image xISO';
  SErrorCrearXISO = 'Image Creation Error.';
  SImagenCreadaOK = 'Image Creation Ok.';
  SParchearXBE = 'XBE Patch Error.';
  SIntroduceDirEx = 'Select the folder where to extract the files.';
  SFinExtraccion = 'End of extraction.';
  SDVDnoXBOX = 'The CD/DVD is not an XBOX CD/DVD.';
  SFiles = 'Files';
  SFolders = 'Folders';
  SISOSize = 'ISO Size';
  SImagenNoXBOX = 'The file does not correspond with an XISO.';
  SDesarrollado = 'Development by';
  SAcercaDe = 'About xISO';
  SManejoASPI = 'ASPI Managment:';
  SLecturaXBOX = 'Reading XBOX DVD FS:';
  SEscrituraXBOX = 'Making XBOX DVD FS:';
  SExit = 'Exit';
  SCancel = 'Cancel';
  SEsperarHilo = 'Please wait to stop all the processes';
  SCarpetaExtError = 'Introduce the folder where to extract the files';
  SInstalarASPI = 'ASPI drivers not found or the version is 4.7x' + #13 +
    'Please read the README.TXT on how to get them.';
  SISO9660ok = 'Add ISO 9660 support to the image.';


               // xisomaker Texts
  SEscaneandoCarpeta = 'Scanning folders... ';
  SGenerandoEntrada = 'Generating files tree... ';
  SIntroduciendoFichero = 'Adding file... ';
  SParadaAnormal = 'The process has been aborted.';
  SEscritoPVD = 'PVD Writed';
  SEscritoXPVD = 'XDFS PVD Writed';
  SInicioCreacion = 'Beginning image creation...';
  SFinCreacion = 'End of image creation.';

               // Register
  SRegCrearXISO = 'Create xISO';
  SRegExtraerXISO = 'Extract xISO';
  SRegExtraerConXISO = 'Extract with xISO';
  SRegAbrirConXISO = 'Open with xISO';

               //Grabacion
  SGraNoDisco = 'No Disk or Disk Full.';
  SGraDiscoLibre = 'Disk writable.';
  SGraFinGrabacion = '.';
  SGraNoSector2048 = 'The size of the image is not equal to the size if the sectors';

               // Creation ISO (Nuevo)
  SNuevaCarpeta = 'New Folder';
  SNombreCarpeta = 'Dir Name';
  SEtiqueta = 'Label';

implementation

end.
