import os

# 設定
src_directory = './src'
output_filename = './src.txt'

# srcディレクトリが存在するか確認
if not os.path.isdir(src_directory):
    print(f"エラー: ディレクトリ '{src_directory}' が見つかりません。")
    exit()

# 出力ファイルを開く (UTF-8で書き込み)
with open(output_filename, 'w', encoding='utf-8') as outfile:
    # os.walkを使ってディレクトリを再帰的に探索
    for dirpath, dirnames, filenames in os.walk(src_directory):
        # ファイル名順不同にならないようソートして処理
        for filename in sorted(filenames):
            filepath = os.path.join(dirpath, filename)
            
            print(f"処理中: {filepath}")
            
            # ファイルパスを書き込む（相対パスで見やすくする）
            # ./src/subdir/file.txt のような形式になります
            relative_path = os.path.relpath(filepath, start='.')
            outfile.write(f"{relative_path}\n---\n")
            
            # ファイルの内容を読み込んで書き込む
            try:
                with open(filepath, 'r', encoding='utf-8') as infile:
                    content = infile.read()
                    outfile.write(content)
            except UnicodeDecodeError:
                 outfile.write(f"\n--- エラー: ファイル '{relative_path}' はUTF-8でデコードできませんでした（バイナリファイルの可能性があります） ---\n")
            except Exception as e:
                outfile.write(f"\n--- エラー: ファイル '{relative_path}' を読み込めませんでした: {e} ---\n")

            # 内容と次のファイルの間に区切り線を入れる
            outfile.write("\n---\n\n")

print(f"全てのファイルを '{output_filename}' にまとめました。")