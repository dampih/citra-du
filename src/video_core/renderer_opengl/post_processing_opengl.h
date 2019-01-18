// Copyright 2014 Citra Emulator Project
// Licensed under GPLv2 or any later version
// Refer to the license.txt file included.

#pragma once

#include <string>
#include <vector>

namespace OpenGL {

std::vector<std::string> GetPostProcessingShaders(bool anaglyph);
std::string GetShader(bool anaglyph, std::string shader);

} // namespace OpenGL
