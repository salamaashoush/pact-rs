#!/usr/bin/env python3
import os
import glob

def fix_encoding_in_file(filepath):
    """Fix encoding issues by replacing smart quotes and dashes with ASCII equivalents."""
    try:
        with open(filepath, 'r', encoding='utf-8') as f:
            content = f.read()
        
        # Replace smart quotes and dashes with ASCII equivalents
        replacements = {
            '—': '-',  # em-dash
            '–': '-',  # en-dash
            ''': "'",  # left single quotation mark
            ''': "'",  # right single quotation mark
            '"': '"',  # left double quotation mark
            '"': '"',  # right double quotation mark
            '…': '...',  # horizontal ellipsis
        }
        
        for smart_char, ascii_char in replacements.items():
            content = content.replace(smart_char, ascii_char)
        
        with open(filepath, 'w', encoding='utf-8') as f:
            f.write(content)
        
        print(f"Fixed encoding in: {filepath}")
        
    except Exception as e:
        print(f"Error processing {filepath}: {e}")

def main():
    """Fix encoding issues in all markdown files in docs/builtins/."""
    pattern = "docs/builtins/**/*.md"
    files = glob.glob(pattern, recursive=True)
    
    print(f"Found {len(files)} markdown files to process")
    
    for filepath in files:
        fix_encoding_in_file(filepath)
    
    print("Encoding fix complete!")

if __name__ == "__main__":
    main() 
