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
    # ディレクトリ内のすべてのファイル名を取得
    for filename in sorted(os.listdir(src_directory)):
        filepath = os.path.join(src_directory, filename)
        
        # ファイルであるかを確認
        if os.path.isfile(filepath):
            print(f"処理中: {filepath}")
            # ファイル名を書き込む
            outfile.write(f"{filepath}\n---\n")
            
            # ファイルの内容を読み込んで書き込む
            try:
                with open(filepath, 'r', encoding='utf-8') as infile:
                    content = infile.read()
                    outfile.write(content)
            except Exception as e:
                outfile.write(f"\n--- エラー: ファイル '{filepath}' を読み込めませんでした: {e} ---\n")

            # 内容と区切り線の間に改行を入れ、区切り線を書き込む
            outfile.write("\n---\n")

print(f"全てのファイルを '{output_filename}' にまとめました。")