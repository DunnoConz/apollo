import { createServer } from 'http'
import { exec } from 'child_process'
import { promisify } from 'util'
import { writeFile, unlink } from 'fs/promises'
import { join } from 'path'
import { tmpdir } from 'os'

const execAsync = promisify(exec)

const server = createServer(async (req, res) => {
  if (req.method === 'POST' && req.url === '/api/compile') {
    try {
      let body = ''
      for await (const chunk of req) {
        body += chunk
      }
      
      const { code } = JSON.parse(body)
      
      // Create a temporary file
      const tempFile = join(tmpdir(), `apollo-playground-${Date.now()}.rkt`)
      await writeFile(tempFile, code)
      
      try {
        // Compile the code
        const { stdout, stderr } = await execAsync(`raco exe ${tempFile}`)
        
        // Get the Luau output
        const { stdout: luauOutput } = await execAsync(`./apollo-bin ${tempFile} -o -`)
        
        res.writeHead(200, { 'Content-Type': 'application/json' })
        res.end(JSON.stringify({
          output: stdout || stderr,
          luau: luauOutput
        }))
      } finally {
        // Clean up the temporary file
        await unlink(tempFile)
      }
    } catch (error) {
      res.writeHead(500, { 'Content-Type': 'application/json' })
      res.end(JSON.stringify({ error: error.message }))
    }
  } else {
    res.writeHead(404)
    res.end()
  }
})

export default server 