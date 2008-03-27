{
 Unidad: XBE.PAS
 Autor: Yursoft
 Descripcion: Lee la estructura de un ejecutable de XBOX (XBE)
}

unit xbe;

interface

uses windows, classes, sysutils, dialogs;

const
  XBEH = 'XBEH';

type
{$A-}
  TCabecera = record
    XBEH: array[0..3] of char;
    FirmaDigital: array[0..255] of byte;
    dBaseXBE: DWORD; // $10000 normalmente
    tCabeceras: DWORD;
    tImagen: DWORD;
    tCabeceraImagen: DWORD; //$178 normalmente
    FechaHora: DWORD;
    pDireccionCertificado: DWORD; // Puntero a la estructura de certificacion
    NumeroSecciones: DWORD;
    pSeccionCabeceras: DWORD;
    BanderasInicio: DWORD;
    pEntryPoint: DWORD; // XOReadas con PUBKEY[$80] y PUBKEY[$90]
    pHiloDirectorio: DWORD; //Direccion al Hilo del Directorio de almacenamiento local
    tPila: DWORD; // Tamaño Pila (Copia PE)
    tMonticuloReserva: DWORD; // Tamaño Monticulo de Reserva (Copia PE)
    tMonticulo: DWORD; // Tamaño Monticulo (Copia PE)
    pDirBaseOriginal: DWORD; // Puntero a direccion base original
    tImagenOriginal: DWORD; // (Copia PE)
    ChecksumOriginal: DWORD; // (Copia PE)
    FechaHoraOriginal: DWORD; // (Copia PE)
    pNombreRutaDebug: DWORD; //Puntero a un char
    pNombreFicheroDebug: DWORD;
    pUnicodeNombreFichero: DWORD;
    pImagenKernel: DWORD; // XOReada con PUBKEY[$84] y PUBKEY[$88]
    pNoImportarDirectorioKernel: DWORD;
    dNumeroVersionLibreria: DWORD;
    pVersionLibreria: DWORD;
    pVersionKernel: DWORD;
    pXAPI: DWORD;
    pLogoBMP: DWORD;
    tLogoBMP: DWORD;
  end;

  TCertificado = record
    tCertificado: DWORD;
    FechaHora: DWORD;
    IDTitulo: DWORD;
    NombreJuego: array[0..$50 - 1] of Char;
    IDsAlternativos: array[0..15] of DWORD;
    MediosPermitidos: DWORD;
    Regiones: DWORD;
    Calificacion: DWORD;
    NumeroDisco: DWORD;
    Version: DWORD;
    LANKey: array[0..15] of byte;
    SignatureKey: array[0..15] of byte;
    KeysAlternativos: array[0..15] of array[0..15] of byte;
  end;

  TXBE = record
    Cabecera: TCabecera;
    Certificado: TCertificado;
  end;
{$A+}

function LeerXBE(sXBE: string; var pXBE: TXBE): boolean;

implementation

//--- 4 Bytes a DWORD formato Motorola

function mByte2DWORD(b3, b2, b1, b0: byte): Dword;
begin
  result := ((LongInt(b3) shl 24) and $FF000000) or
    ((LongInt(b2) shl 16) and $00FF0000) or
    ((LongInt(b1) shl 8) and $0000FF00) or
    ((LongInt(b0)) and $000000FF);
end;

//--- 4 Bytes a DWORD formato Intel

function iByte2DWORD(b3, b2, b1, b0: byte): Dword;
begin
  result := ((LongInt(b0) shl 24) and $FF000000) or
    ((LongInt(b1) shl 16) and $00FF0000) or
    ((LongInt(b2) shl 8) and $0000FF00) or
    ((LongInt(b3)) and $000000FF);
end;


function LeerXBE(sXBE: string; var pXBE: TXBE): boolean;
var
  fXBE: TFilestream;
begin
  fXBE := TFilestream.Create(sXBE, fmOpenReadWrite);
  fXBE.Read(pXBE.Cabecera, sizeof(pXBE.Cabecera));
  fXBE.Seek(pXBE.Cabecera.pDireccionCertificado - pXBE.Cabecera.dBaseXBE, soBeginning);
  fXBE.Read(pXBE.Certificado, sizeof(pXBE.Certificado));
  fXBE.Free;
  result := True;
end;

end.

