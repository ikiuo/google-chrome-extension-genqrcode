# QRコードのGIF画像生成

文字列からQRコードのGIF画像を生成する Google Chrome 拡張機能です。

## インストール

1. 設定 ([chrome://settings/](chrome://settings/)) を開く
1. 拡張機能([chrome://extensions/](chrome://extensions/)) を開く
1. 「デベロッパー モード」をONにする
1. 「パッケージ化されていない拡張機能を読み込む」をクリック
1. このディレクトリを選択する

## 使用方法

拡張機能のアイコンをクリックすると、現在のページのQRコードを生成します。

画像は ドラッグ＆ドロップ や 右クリック メニュー などを使って取得できます。

文字列の入力領域は編集可能で、最初に開いているページの URL が入っています。

設定を開くと、拡大率・既定文字・訂正能力を変更できます。

文字列が長すぎるとQRコードを生成できません。

## 参考

JavaScript の殆どはこちらのプログラムです。

https://github.com/ikiuo/dhtml-qrcode
