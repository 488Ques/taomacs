"""rass preset for taomacs TypeScript buffers.

Servers come from the command line; this preset only supplies the logic.
The bundled tslint logic answers ESLint's configuration request with
validate=probe and no codeActionOnSave, under which the ESLint server
offers no source.fixAll.eslint action.  Use VS Code's effective values.
"""

from rassumfrassum.presets import tslint


class TaomacsTypeScriptLogic(tslint.TypeScriptLogic):
    async def on_client_response(
        self, method, request_params, response_payload, is_error, server
    ):
        await super().on_client_response(
            method, request_params, response_payload, is_error, server
        )
        if (
            method == 'workspace/configuration'
            and not is_error
            and 'eslint' in server.name.lower()
        ):
            for item in response_payload:
                if isinstance(item, dict) and 'validate' in item:
                    item['validate'] = 'on'
                    item.setdefault('codeActionOnSave', {'mode': 'all'})


def logic_class():
    return TaomacsTypeScriptLogic
