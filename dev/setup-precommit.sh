#!/bin/bash
# Setup script for pre-commit framework
# Alternative til manuel git hook

echo "🔧 Setting up pre-commit framework for SPC App..."

# Check if pre-commit is installed
if ! command -v pre-commit &> /dev/null; then
    echo "❌ pre-commit not found"
    echo ""
    echo "📥 Install pre-commit first:"
    echo "   # Via pip:"
    echo "   pip install pre-commit"
    echo ""
    echo "   # Via homebrew (macOS):"
    echo "   brew install pre-commit"
    echo ""
    echo "   # Via conda:"
    echo "   conda install -c conda-forge pre-commit"
    exit 1
fi

echo "✅ pre-commit found: $(pre-commit --version)"

# Check if .pre-commit-config.yaml exists
if [ ! -f ".pre-commit-config.yaml" ]; then
    echo "❌ .pre-commit-config.yaml not found"
    echo "   Make sure you're in the project root directory"
    exit 1
fi

echo "📋 Installing pre-commit hooks..."
if pre-commit install; then
    echo "✅ Pre-commit hooks installed successfully!"
else
    echo "❌ Failed to install pre-commit hooks"
    exit 1
fi

# Test the setup
echo ""
echo "🧪 Testing pre-commit setup..."
if pre-commit run --all-files; then
    echo ""
    echo "🎉 Pre-commit framework setup complete!"
    echo ""
    echo "📋 Usage:"
    echo "   - Hooks run automatically on git commit"
    echo "   - Run manually: pre-commit run --all-files"
    echo "   - Update hooks: pre-commit autoupdate"
    echo ""
    echo "⚠️  Note: This replaces the manual git hook"
    echo "   Remove .git/hooks/pre-commit if you want to use pre-commit framework exclusively"
else
    echo ""
    echo "⚠️  Pre-commit setup complete, but some checks failed"
    echo "   Fix the issues and run: pre-commit run --all-files"
fi